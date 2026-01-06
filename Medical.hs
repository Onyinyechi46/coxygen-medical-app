{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>), take)
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum & Redeemer
------------------------------------------------------------------------

data LoanDatum = LoanDatum
    {mdPatient     :: PubKeyHash
    , mdProvider    :: PubKeyHash
    , mdBillAmount  :: Integer      -- total ADA of medical bill
    , mdCreditLimit :: Integer      -- max ADA that can be covered on credit
    , mdInterest    :: Integer      -- interest on credit (if any)
    }
PlutusTx.unstableMakeIsData ''MedicalDatum

data MedicalAction
    = OpenCredit     -- Patient opens credit to pay medical bills
    | RepayCredit    -- Patient repays the credit + interest

PlutusTx.unstableMakeIsData ''MedicalAction
------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkLoanValidator #-}
mkMedicalValidator :: MedicalDatum -> MedicalAction -> ScriptContext -> Bool
mkMedicalValidator dat action ctx =
    case action of

        ----------------------------------------------------------------
        -- Patient opens credit to pay medical bills
        ----------------------------------------------------------------
         OpenCredit ->
            traceIfFalse "patient not signed" patientSigned &&
            traceIfFalse "credit limit exceeded" validCredit

        ----------------------------------------------------------------
        -- Patient repays provider
        ----------------------------------------------------------------
        RepayCredit ->
            traceIfFalse "patient not signed" payerSigned &&
            traceIfFalse "provider not paid" providerPaid &&
            traceIfFalse "collateral not returned" collateralReturned
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    patientSigned :: Bool
    patientSigned =
        signedBy (mdPatient dat) ctx


    ----------------------------------------------------------------
    -- LTV: principal <= collateral * LTV / 100
    ----------------------------------------------------------------
    validLTV :: Bool
    validLTV =
        ldPrincipal dat * 100 <= ldCollateral dat * ldLTV dat

    ----------------------------------------------------------------
    --  Provider must receive bill + interest
    ----------------------------------------------------------------
    providerPaid :: Bool
    providerPaid =
        let paid =
                valueOf
                    (valuePaidTo info (mdProvider dat))
                    adaSymbol
                    adaToken
        in paid >= (mdBillAmount dat + ldInterest dat)

    ----------------------------------------------------------------
    -- Script must release full collateral
    ----------------------------------------------------------------
    collateralReturned :: Bool
    collateralReturned =
        case findOwnInput ctx of
            Nothing -> traceError "no script input"
            Just i  ->
                let inVal = txOutValue (txInInfoResolved i)
                in valueOf inVal adaSymbol adaToken == ldCollateral dat

------------------------------------------------------------------------
-- Untyped Wrapper
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkLoanValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator =
    mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash & Script Address
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes  = Serialise.serialise val
        short  = SBS.toShort (LBS.toStrict bytes)
    in PlutusV2.ValidatorHash (toBuiltin (SBS.fromShort short))

plutusScriptAddress :: Address
plutusScriptAddress =
    Address
        (ScriptCredential (plutusValidatorHash validator))
        Nothing

------------------------------------------------------------------------
-- Bech32 Script Address (Off-chain)
------------------------------------------------------------------------

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

-----------------------------------------------------------------------------------
-- CBOR HEX
-----------------------------------------------------------------------------------

validatorToCBORHex :: Validator -> String
validatorToCBORHex val =
    let bytes = LBS.toStrict $ Serialise.serialise val
    in BS.foldr (\b acc -> byteToHex b <> acc) "" bytes
  where
    hexChars = "0123456789abcdef"
    byteToHex b =
        let hi = P.fromIntegral b `P.div` 16
            lo = P.fromIntegral b `P.mod` 16
        in [ hexChars P.!! hi, hexChars P.!! lo ]

------------------------------------------------------------------------
-- File Writer
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
        hex   = B16.encode bytes
    BS.writeFile path hex
    putStrLn $ "CBOR hex written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "ada_lending.plutus" validator
    writeCBOR      "collaterize.cbor"   validator

    let vh      = plutusValidatorHash validator
        addr    = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator
        cborHex = validatorToCBORHex validator

    putStrLn "\n--- ADA Lending Protocol ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Script Address: " <> P.show addr
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn $ "CBOR Hex (first 120 chars): " <> P.take 120 cborHex <> "..."
    putStrLn "----------------------------"
