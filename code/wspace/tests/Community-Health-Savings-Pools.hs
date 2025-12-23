{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE NumericUnderscores  #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T
import System.IO (hSetEncoding, stdout, utf8)

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo, txInfoValidRange, scriptContextTxInfo, findOwnInput)
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval (contains, from, to)
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless, divide)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- DATA TYPES: Community Health Savings Pool
------------------------------------------------------------------------

-- | Member information
data MemberInfo = MemberInfo
    { miMemberPubKey     :: PubKeyHash    -- Member's public key
    , miTotalContributed :: Integer       -- Total ADA contributed (in Lovelace)
    , miLastContribution :: POSIXTime     -- Timestamp of last contribution
    , miClaimHistory     :: Integer       -- Number of claims made
    , miIsActive         :: Bool          -- Membership status
    }
PlutusTx.unstableMakeIsData ''MemberInfo

-- | Claim request details
data ClaimRequest = ClaimRequest
    { crClaimant       :: PubKeyHash      -- Who is claiming
    , crAmount         :: Integer         -- Amount requested (Lovelace)
    , crHospitalId     :: BuiltinByteString -- Hospital identifier
    , crBillHash       :: BuiltinByteString -- Hash of medical bill (for verification)
    , crSubmissionTime :: POSIXTime       -- When claim was submitted
    , crVotesFor       :: Integer         -- Votes in favor
    , crVotesAgainst   :: Integer         -- Votes against
    , crApproved       :: Bool            -- Claim approval status
    , crProcessed      :: Bool            -- Has claim been paid out?
    }
PlutusTx.unstableMakeIsData ''ClaimRequest

-- | Pool state datum
data PoolDatum = PoolDatum
    { pdTotalBalance    :: Integer        -- Current pool balance (Lovelace)
    , pdTotalMembers    :: Integer        -- Number of active members
    , pdMinContribution :: Integer        -- Minimum monthly contribution
    , pdClaimLimit      :: Integer        -- Max claim without voting
    , pdPoolAdmin       :: PubKeyHash     -- Admin for oracle verification
    }
PlutusTx.unstableMakeIsData ''PoolDatum

-- | Actions for the pool
data PoolAction 
    = JoinPool PubKeyHash                 -- New member joins
    | Contribute PubKeyHash Integer       -- Member makes contribution
    | SubmitClaim ClaimRequest            -- Member submits medical claim
    | VoteOnClaim PubKeyHash Integer Bool -- Member votes on claim (claimId, vote)
    | ProcessClaim Integer                -- Pay out approved claim (claimId)
    | VerifyClaim Integer BuiltinByteString -- Admin verifies bill (claimId, proof)
PlutusTx.unstableMakeIsData ''PoolAction

------------------------------------------------------------------------
-- HELPER FUNCTIONS
------------------------------------------------------------------------

{-# INLINABLE getScriptBalance #-}
-- Get the current balance locked in the script
getScriptBalance :: ScriptContext -> Integer
getScriptBalance ctx =
    case findOwnInput ctx of
        Nothing -> traceError "no script input found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v adaSymbol adaToken

{-# INLINABLE hasMinimumContribution #-}
-- Check if contribution meets minimum requirement
hasMinimumContribution :: Integer -> Integer -> Bool
hasMinimumContribution amount minRequired = amount >= minRequired

{-# INLINABLE isClaimEligible #-}
-- Check if claimant has contributed recently (within 3 months)
isClaimEligible :: POSIXTime -> POSIXTime -> Bool
isClaimEligible lastContribution currentTime =
    let threeMonthsInMs = 90 * 24 * 60 * 60 * 1000 :: Integer  -- 90 days in milliseconds
        timeDiff = currentTime - lastContribution
        POSIXTime timeDiffInt = timeDiff  -- Extract Integer from POSIXTime
    in timeDiffInt <= threeMonthsInMs

{-# INLINABLE claimNeedsVoting #-}
-- Check if claim amount requires community voting
claimNeedsVoting :: Integer -> Integer -> Bool
claimNeedsVoting claimAmount claimLimit = claimAmount > claimLimit

{-# INLINABLE hasQuorum #-}
-- Check if voting has reached quorum (>50% of members voted)
hasQuorum :: Integer -> Integer -> Integer -> Bool
hasQuorum votesFor votesAgainst totalMembers =
    let totalVotes = votesFor + votesAgainst
        halfMembers = totalMembers `divide` 2
    in totalVotes > halfMembers

{-# INLINABLE isApproved #-}
-- Check if claim is approved (more yes than no votes)
isApproved :: Integer -> Integer -> Bool
isApproved votesFor votesAgainst = votesFor > votesAgainst

{-# INLINABLE divide #-}
divide :: Integer -> Integer -> Integer
divide a b = Builtins.divideInteger a b

------------------------------------------------------------------------
-- VALIDATOR LOGIC
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: PoolDatum -> PoolAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      -- Member joins the pool
      JoinPool newMember ->
          traceIfFalse "new member signature missing" (txSignedBy info newMember) &&
          traceIfFalse "insufficient joining contribution" 
              (hasMinimumContribution contributionAmount (pdMinContribution dat))

      -- Member makes a contribution
      Contribute member amount ->
          traceIfFalse "member signature missing" (txSignedBy info member) &&
          traceIfFalse "contribution too small" 
              (hasMinimumContribution amount (pdMinContribution dat)) &&
          traceIfFalse "pool balance not increased" 
              (getScriptBalance ctx >= pdTotalBalance dat + amount)

      -- Member submits a medical claim
      SubmitClaim claim ->
          traceIfFalse "claimant signature missing" 
              (txSignedBy info (crClaimant claim)) &&
          traceIfFalse "claimant not eligible (no recent contributions)" 
              eligibleForClaim &&
          traceIfFalse "claim amount exceeds pool balance" 
              (crAmount claim <= pdTotalBalance dat) &&
          traceIfFalse "claim already processed" 
              (not $ crProcessed claim)

      -- Member votes on a claim
      VoteOnClaim voter claimId vote ->
          traceIfFalse "voter signature missing" (txSignedBy info voter) &&
          traceIfFalse "voter is not a member" memberIsActive

      -- Process and pay out an approved claim
      ProcessClaim claimId ->
          traceIfFalse "claim not verified by admin" adminVerified &&
          traceIfFalse "claim requires voting but lacks quorum" votingComplete &&
          traceIfFalse "claim not approved by community" claimApproved &&
          traceIfFalse "claimant not paid correct amount" claimantPaidCorrectly &&
          traceIfFalse "pool balance not reduced" 
              (getScriptBalance ctx <= pdTotalBalance dat - claimAmount)

      -- Admin verifies medical bill authenticity
      VerifyClaim claimId proof ->
          traceIfFalse "admin signature missing" 
              (txSignedBy info (pdPoolAdmin dat)) &&
          traceIfFalse "invalid verification proof" 
              (proof /= emptyByteString)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    currentTime :: POSIXTime
    currentTime = case txInfoValidRange info of
        _ -> 0 -- Simplified for example; extract from interval in production

    -- For demonstration: assume we track contribution amounts
    contributionAmount :: Integer
    contributionAmount = 10_000_000 -- 10 ADA minimum (placeholder)

    -- Check if member contributed within last 3 months
    eligibleForClaim :: Bool
    eligibleForClaim = True -- Simplified; would check MemberInfo in production

    -- Check if member is active
    memberIsActive :: Bool
    memberIsActive = True -- Simplified; would verify membership status

    -- Check admin verification
    adminVerified :: Bool
    adminVerified = True -- Simplified; would check VerifyClaim action was called

    -- Check if voting requirements met
    votingComplete :: Bool
    votingComplete = True -- Simplified; would check quorum + approval

    -- Check if claim approved
    claimApproved :: Bool
    claimApproved = True -- Simplified; would check vote results

    -- Claim amount being processed
    claimAmount :: Integer
    claimAmount = 50_000_000 -- Placeholder; would extract from ClaimRequest

    -- Verify claimant received payment
    claimantPaidCorrectly :: Bool
    claimantPaidCorrectly = True -- Simplified; would verify valuePaidTo

    emptyByteString :: BuiltinByteString
    emptyByteString = Builtins.emptyByteString

------------------------------------------------------------------------
-- BOILERPLATE
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @PoolDatum d
        red = unsafeFromBuiltinData @PoolAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- UTILITY FUNCTIONS
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
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
        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------

main :: IO ()
main = do
    hSetEncoding stdout utf8
    
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "chsp-pool.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Community Health Savings Pool (CHSP) ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Address: " <> P.show onchain
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn "-------------------------------------------"
    putStrLn "✓ CHSP smart contract generated successfully!"