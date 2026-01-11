{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
-- DATUM + REDEEMER
------------------------------------------------------------------------

data SubsidyDatum = SubsidyDatum
    { sdFarmerPKH       :: PubKeyHash
    , sdGovPKH          :: PubKeyHash
    , sdRequiredAmount :: Integer
    , sdRainOK         :: Bool
    , sdCropOK         :: Bool
    , sdCommunityOK    :: Bool
    }
PlutusTx.unstableMakeIsData ''SubsidyDatum


data SubsidyAction
    = ClaimSubsidy
    | ReportFraud
    | UpdateData
PlutusTx.unstableMakeIsData ''SubsidyAction


------------------------------------------------------------------------
-- VALIDATOR
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: SubsidyDatum -> SubsidyAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of

      ClaimSubsidy ->
            traceIfFalse "farmer signature missing"
                (txSignedBy info (sdFarmerPKH dat))
        &&  traceIfFalse "rainfall condition failed"
                (sdRainOK dat)
        &&  traceIfFalse "crop planting verification failed"
                (sdCropOK dat)
        &&  traceIfFalse "community attestation missing"
                (sdCommunityOK dat)
        &&  traceIfFalse "farmer not paid correctly"
                farmerPaidCorrectly

      ReportFraud ->
            traceIfFalse "government signature missing"
                (txSignedBy info (sdGovPKH dat))
        &&  traceIfFalse "government did not receive reclaimed funds"
                govGetsFunds

      UpdateData ->
            traceIfFalse "government signature missing"
                (txSignedBy info (sdGovPKH dat))

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    farmerPaidCorrectly :: Bool
    farmerPaidCorrectly =
        let v = valuePaidTo info (sdFarmerPKH dat)
        in valueOf v adaSymbol adaToken >= sdRequiredAmount dat

    govGetsFunds :: Bool
    govGetsFunds =
        let v = valuePaidTo info (sdGovPKH dat)
        in valueOf v adaSymbol adaToken >= sdRequiredAmount dat


------------------------------------------------------------------------
-- UNTYPED WRAPPER
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @SubsidyDatum d
        red = unsafeFromBuiltinData @SubsidyAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()


validator :: Validator
validator =
    mkValidatorScript
        $$(PlutusTx.compile [|| mkValidatorUntyped ||])


------------------------------------------------------------------------
-- HASH + ADDRESS
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash v =
    let bytes    = Serialise.serialise v
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin


plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing


toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val

        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised

        scriptHash =
            C.hashScript
                (C.PlutusScript C.PlutusScriptV2 plutusScript)

        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress

    in T.unpack (C.serialiseAddress shelleyAddr)


------------------------------------------------------------------------
-- CBOR HEX
------------------------------------------------------------------------

validatorToCBORHex :: Validator -> String
validatorToCBORHex =
    T.unpack
    . T.decodeUtf8
    . B16.encode
    . LBS.toStrict
    . Serialise.serialise


------------------------------------------------------------------------
-- WRITE FILE
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
-- MAIN
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
        cborHex = validatorToCBORHex validator

    writeValidator "subsidy.plutus" validator
    writeCBOR      "subsidy.cbor"   validator

    putStrLn "\n--- Subsidy Transparency Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show (plutusValidatorHash validator)
    putStrLn $ "Plutus Address:          " <> P.show plutusScriptAddress
    putStrLn $ "Bech32 Address:          " <> toBech32ScriptAddress network validator
    putStrLn $ "CBOR (preview):          " <> P.take 80 cborHex <> "..."
    putStrLn "------------------------------------------------"
    putStrLn "Subsidy transparency validator generated successfully."

