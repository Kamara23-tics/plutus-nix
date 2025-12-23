{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE NumericUnderscores   #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- System.IO import for encoding fix
import System.IO (hSetEncoding, stdout, utf8)

-- Plutus core
import Plutus.V2.Ledger.Api (POSIXTime, POSIXTimeRange, ScriptContext(..), TxInfo(..), TxOut(..), TxInInfo(..), Datum(..), Redeemer(..), CurrencySymbol, TokenName, Validator, ValidatorHash, Address(Address), PubKeyHash, BuiltinData, mkValidatorScript, unsafeFromBuiltinData, Credential(ScriptCredential))
import Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo, txInfoValidRange, scriptContextTxInfo, findOwnInput)
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval (contains, from, ivTo, UpperBound(..), Extended(..))
import Plutus.V1.Ledger.Value (adaSymbol, adaToken, valueOf) 
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless, divide) 
import qualified PlutusTx.Builtins as Builtins

-- Serialization & Cardano API (Updated Imports)
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS
-- NEW IMPORT for PlutusScript and TextEnvelope handling
import Cardano.Api (PlutusScript, PlutusScriptV2) 

------------------------------------------------------------------------
-- DATA TYPES (PGEF Staking Vault)
------------------------------------------------------------------------

-- | Staking Pool State / Datum
data StakingDatum = StakingDatum
    { sdStaker       :: PubKeyHash
    , sdPrincipal    :: Integer
    , sdStartTime    :: POSIXTime
    , sdClaimedYield :: Integer
    , sdActive       :: Bool
    }
PlutusTx.unstableMakeIsData ''StakingDatum

-- | Actions for the Validator
data StakingAction = Stake | ClaimAndUnstake
PlutusTx.unstableMakeIsData ''StakingAction

------------------------------------------------------------------------
-- HELPER FUNCTIONS (Omitted for brevity, assumed unchanged)
------------------------------------------------------------------------

{-# INLINABLE unPosixTimeHelper #-}
unPosixTimeHelper :: POSIXTime -> Integer
unPosixTimeHelper (PlutusV2.POSIXTime i) = i

{-# INLINABLE integerDivide #-}
integerDivide :: Integer -> Integer -> Integer
integerDivide a b = Builtins.divideInteger a b

{-# INLINABLE calculateAccruedYield #-}
calculateAccruedYield :: StakingDatum -> POSIXTime -> Integer
calculateAccruedYield dat currentTime =
    let duration = currentTime - sdStartTime dat    
        secondsInYear = 31_536_000                 
        yieldRate = 500                            
        principal = sdPrincipal dat
        
        durationAsSeconds = unPosixTimeHelper duration `integerDivide` 1_000_000
        
        numerator = Builtins.multiplyInteger (Builtins.multiplyInteger principal durationAsSeconds) yieldRate
        denominator = Builtins.multiplyInteger 10_000 secondsInYear
    in numerator `integerDivide` denominator

{-# INLINABLE getCurrentTime #-}
getCurrentTime :: POSIXTimeRange -> POSIXTime
getCurrentTime txRange = 
    case ivTo txRange of
        UpperBound (Finite t) _ -> t
        _ -> traceError "cannot determine current time from tx valid range"

------------------------------------------------------------------------
-- VALIDATOR LOGIC (Omitted for brevity, assumed unchanged)
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: StakingDatum -> StakingAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      Stake -> 
          traceIfFalse "staker signature missing" (txSignedBy info (sdStaker dat)) &&
          traceIfFalse "stake must be active"     (sdActive dat) &&
          traceIfFalse "principal must be > 0"    (sdPrincipal dat > 0)

      ClaimAndUnstake ->
          traceIfFalse "staker signature missing" (txSignedBy info (sdStaker dat)) &&
          traceIfFalse "stake must be active"     (sdActive dat) &&
          traceIfFalse "staker not paid correct amount" stakerPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    currentTime :: POSIXTime
    currentTime = getCurrentTime (txInfoValidRange info)

    accruedYield :: Integer
    accruedYield = calculateAccruedYield dat currentTime

    totalPayout :: Integer
    totalPayout = sdPrincipal dat + accruedYield - sdClaimedYield dat

    stakerPaid :: Bool
    stakerPaid =
        let v = valuePaidTo info (sdStaker dat)
        in valueOf v adaSymbol adaToken >= totalPayout

------------------------------------------------------------------------
-- BOILERPLATE (Omitted for brevity, assumed unchanged)
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @StakingDatum d
        red = unsafeFromBuiltinData @StakingAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- UTILITY FUNCTIONS (Omitted for brevity, assumed unchanged)
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

-- MODIFIED: Function now uses Cardano API to output the TextEnvelope (JSON) format
writePlutusScript :: FilePath -> Validator -> IO (Either (CS.FileError ()) ())
writePlutusScript path val = do
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: PlutusScript PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
    
    -- Use writeFileTextEnvelope to write the standard JSON format
    CS.writeFileTextEnvelope path Nothing plutusScript

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    result <- writePlutusScript path val
    case result of
        Left err -> putStrLn $ "Error writing validator: " <> P.show err
        Right () -> putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    hSetEncoding stdout utf8
    
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "pgef-staking-vault.plutus" validator

    let vh      = plutusValidatorHash validator
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- PGEF Staking Vault Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "-----------------------------------------"
    putStrLn "PGEF Staking Vault validator generated successfully."