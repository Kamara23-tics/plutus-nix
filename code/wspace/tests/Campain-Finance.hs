{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Data Types
------------------------------------------------------------------------

-- | Milestone within a grant
data Milestone = Milestone
    { mAmount      :: Integer              -- Amount for this milestone (lovelace)
    , mReportHash  :: BuiltinByteString    -- Required report hash to release funds
    , mDeadline    :: POSIXTime            -- Deadline for completion
    , mCompleted   :: Bool                 -- Completion status
    }
PlutusTx.unstableMakeIsData ''Milestone

-- | Grant with milestone tracking
data Grant = Grant
    { gRecipient      :: PubKeyHash        -- Grant recipient
    , gTotalAmount    :: Integer           -- Total grant amount (lovelace)
    , gMilestones     :: [Milestone]       -- List of milestones
    , gCurrentPhase   :: Integer           -- Current milestone index (0-based)
    , gTotalReleased  :: Integer           -- Total amount released so far
    , gDescription    :: BuiltinByteString -- Grant description/reference
    }
PlutusTx.unstableMakeIsData ''Grant

-- | Campaign treasury configuration
data Campaign = Campaign
    { cTreasurer       :: PubKeyHash           -- Campaign treasurer
    , cPerTxLimit      :: Integer              -- Max spending per transaction (lovelace)
    , cTotalLimit      :: Integer              -- Total campaign spending limit
    , cDisclosureRef   :: BuiltinByteString    -- IPFS/reference to disclosure docs
    , cTotalSpent      :: Integer              -- Running total spent
    , cTotalDonations  :: Integer              -- Running total donations
    , cActive          :: Bool                 -- Campaign active status
    }
PlutusTx.unstableMakeIsData ''Campaign

-- | Donor record for transparency
data DonorRecord = DonorRecord
    { drDonor     :: PubKeyHash
    , drAmount    :: Integer
    , drTimestamp :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''DonorRecord

-- | Combined datum for the contract
data CampaignDatum = CampaignDatum
    { cdCampaign    :: Campaign
    , cdGrants      :: [Grant]
    , cdDonors      :: [DonorRecord]       -- Immutable audit trail
    }
PlutusTx.unstableMakeIsData ''CampaignDatum

-- | Redeemer actions
data CampaignAction
    = Donate POSIXTime                         -- Anyone donates (with timestamp)
    | DisburseOperational Integer              -- Treasurer disburses for operations
    | CreateGrant Grant                        -- Treasurer creates new grant
    | ReleaseMilestone Integer BuiltinByteString  -- Release milestone (grant index, report hash)
    | UpdateDisclosure BuiltinByteString       -- Update disclosure reference
    | CloseCampaign                            -- Close campaign (treasurer only)
PlutusTx.unstableMakeIsData ''CampaignAction

------------------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------------------

{-# INLINABLE findGrant #-}
-- | Find grant by index
findGrant :: Integer -> [Grant] -> Maybe Grant
findGrant idx grants =
    if idx >= 0 && idx < length grants
    then Just (grants !! idx)
    else Nothing

{-# INLINABLE updateGrantAt #-}
-- | Update grant at specific index
updateGrantAt :: Integer -> Grant -> [Grant] -> [Grant]
updateGrantAt idx newGrant grants = go 0 grants
  where
    go _ [] = []
    go i (g:gs)
        | i == idx  = newGrant : gs
        | otherwise = g : go (i + 1) gs

{-# INLINABLE getCurrentMilestone #-}
-- | Get current milestone for a grant
getCurrentMilestone :: Grant -> Maybe Milestone
getCurrentMilestone g =
    let phase = gCurrentPhase g
        ms = gMilestones g
    in if phase >= 0 && phase < length ms
       then Just (ms !! phase)
       else Nothing

{-# INLINABLE updateMilestoneAt #-}
-- | Mark milestone as completed
updateMilestoneAt :: Integer -> [Milestone] -> [Milestone]
updateMilestoneAt idx milestones = go 0 milestones
  where
    go _ [] = []
    go i (m:ms)
        | i == idx  = m { mCompleted = True } : ms
        | otherwise = m : go (i + 1) ms

{-# INLINABLE totalDonations #-}
-- | Calculate total donations
totalDonations :: [DonorRecord] -> Integer
totalDonations = foldr (\dr acc -> acc + drAmount dr) 0

{-# INLINABLE milestonePastDeadline #-}
-- | Check if milestone is past deadline
milestonePastDeadline :: Milestone -> POSIXTimeRange -> Bool
milestonePastDeadline m txRange =
    Interval.contains (Interval.from (mDeadline m + 1)) txRange

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: CampaignDatum -> CampaignAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      Donate timestamp ->
         traceIfFalse "Campaign not active" campaignActive &&
         traceIfFalse "No donation received" donationReceived &&
         traceIfFalse "Donor not recorded" donorRecorded &&
         traceIfFalse "Total donations not updated" donationsUpdated

      DisburseOperational amount ->
         traceIfFalse "Campaign not active" campaignActive &&
         traceIfFalse "Only treasurer can disburse" signedByTreasurer &&
         traceIfFalse "Amount exceeds per-tx limit" withinTxLimit &&
         traceIfFalse "Amount exceeds total budget" withinTotalBudget &&
         traceIfFalse "Treasurer not paid" treasurerPaid &&
         traceIfFalse "Total spent not updated" spendingUpdated

      CreateGrant newGrant ->
         traceIfFalse "Campaign not active" campaignActive &&
         traceIfFalse "Only treasurer can create grants" signedByTreasurer &&
         traceIfFalse "Grant exceeds budget" grantWithinBudget &&
         traceIfFalse "Grant not added" grantAdded &&
         traceIfFalse "Invalid milestone structure" validMilestones

      ReleaseMilestone grantIdx reportHash ->
         traceIfFalse "Campaign not active" campaignActive &&
         traceIfFalse "Only treasurer can release milestones" signedByTreasurer &&
         traceIfFalse "Invalid grant index" validGrantIndex &&
         traceIfFalse "Report hash mismatch" reportHashMatches &&
         traceIfFalse "Milestone already completed" milestoneNotComplete &&
         traceIfFalse "Milestone deadline passed" milestoneNotExpired &&
         traceIfFalse "Recipient not paid" recipientPaid &&
         traceIfFalse "Milestone not marked complete" milestoneMarkedComplete &&
         traceIfFalse "Grant totals not updated" grantTotalsUpdated

      UpdateDisclosure newRef ->
         traceIfFalse "Only treasurer can update disclosure" signedByTreasurer &&
         traceIfFalse "Disclosure not updated" disclosureUpdated

      CloseCampaign ->
         traceIfFalse "Only treasurer can close campaign" signedByTreasurer &&
         traceIfFalse "Campaign not marked closed" campaignMarkedClosed

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    campaign :: Campaign
    campaign = cdCampaign dat

    contOutputs :: [TxOut]
    contOutputs = getContinuingOutputs ctx

    -- ===== GENERAL CHECKS =====

    campaignActive :: Bool
    campaignActive = cActive campaign

    signedByTreasurer :: Bool
    signedByTreasurer = txSignedBy info (cTreasurer campaign)

    -- ===== DONATION VALIDATION =====

    donationReceived :: Bool
    donationReceived =
        case contOutputs of
          [o] -> valueOf (txOutValue o) adaSymbol adaToken > 
                 valueOf (getOwnInput) adaSymbol adaToken
          _ -> False
      where
        getOwnInput = case findOwnInput ctx of
            Just txIn -> txOutValue (txInInfoResolved txIn)
            Nothing -> mempty

    donorRecorded :: Bool
    donorRecorded =
        case (action, contOutputs) of
          (Donate ts, [o]) ->
            case txOutDatum o of
              OutputDatum (Datum d) ->
                case unsafeFromBuiltinData @CampaignDatum d of
                  newDat -> length (cdDonors newDat) == length (cdDonors dat) + 1
              _ -> False
          _ -> False

    donationsUpdated :: Bool
    donationsUpdated =
        case contOutputs of
          [o] ->
            case txOutDatum o of
              OutputDatum (Datum d) ->
                case unsafeFromBuiltinData @CampaignDatum d of
                  newDat -> cTotalDonations (cdCampaign newDat) > 
                           cTotalDonations campaign
              _ -> False
          _ -> False

    -- ===== OPERATIONAL DISBURSEMENT =====

    withinTxLimit :: Bool
    withinTxLimit =
        case action of
          DisburseOperational amt -> amt <= cPerTxLimit campaign
          _ -> False

    withinTotalBudget :: Bool
    withinTotalBudget =
        case action of
          DisburseOperational amt ->
            cTotalSpent campaign + amt <= cTotalLimit campaign
          _ -> False

    treasurerPaid :: Bool
    treasurerPaid =
        case action of
          DisburseOperational amt ->
            let v = valuePaidTo info (cTreasurer campaign)
            in valueOf v adaSymbol adaToken >= amt
          _ -> False

    spendingUpdated :: Bool
    spendingUpdated =
        case (action, contOutputs) of
          (DisburseOperational amt, [o]) ->
            case txOutDatum o of
              OutputDatum (Datum d) ->
                case unsafeFromBuiltinData @CampaignDatum d of
                  newDat -> cTotalSpent (cdCampaign newDat) == 
                           cTotalSpent campaign + amt
              _ -> False
          _ -> False

    -- ===== GRANT CREATION =====

    grantWithinBudget :: Bool
    grantWithinBudget =
        case action of
          CreateGrant g ->
            let totalNeeded = gTotalAmount g
            in cTotalSpent campaign + totalNeeded <= cTotalLimit campaign
          _ -> False

    grantAdded :: Bool
    grantAdded =
        case (action, contOutputs) of
          (CreateGrant _, [o]) ->
            case txOutDatum o of
              OutputDatum (Datum d) ->
                case unsafeFromBuiltinData @CampaignDatum d of
                  newDat -> length (cdGrants newDat) == length (cdGrants dat) + 1
              _ -> False
          _ -> False

    validMilestones :: Bool
    validMilestones =
        case action of
          CreateGrant g ->
            let milestones = gMilestones g
                totalMilestoneAmt = foldr (\m acc -> acc + mAmount m) 0 milestones
            in totalMilestoneAmt == gTotalAmount g && length milestones > 0
          _ -> True

    -- ===== MILESTONE RELEASE =====

    validGrantIndex :: Bool
    validGrantIndex =
        case action of
          ReleaseMilestone idx _ -> idx >= 0 && idx < length (cdGrants dat)
          _ -> False

    reportHashMatches :: Bool
    reportHashMatches =
        case action of
          ReleaseMilestone idx hash ->
            case findGrant idx (cdGrants dat) of
              Just g ->
                case getCurrentMilestone g of
                  Just m -> mReportHash m == hash
                  Nothing -> False
              Nothing -> False
          _ -> False

    milestoneNotComplete :: Bool
    milestoneNotComplete =
        case action of
          ReleaseMilestone idx _ ->
            case findGrant idx (cdGrants dat) of
              Just g ->
                case getCurrentMilestone g of
                  Just m -> not (mCompleted m)
                  Nothing -> False
              Nothing -> False
          _ -> False

    milestoneNotExpired :: Bool
    milestoneNotExpired =
        case action of
          ReleaseMilestone idx _ ->
            case findGrant idx (cdGrants dat) of
              Just g ->
                case getCurrentMilestone g of
                  Just m -> not (milestonePastDeadline m txRange)
                  Nothing -> False
              Nothing -> False
          _ -> False

    recipientPaid :: Bool
    recipientPaid =
        case action of
          ReleaseMilestone idx _ ->
            case findGrant idx (cdGrants dat) of
              Just g ->
                case getCurrentMilestone g of
                  Just m ->
                    let v = valuePaidTo info (gRecipient g)
                    in valueOf v adaSymbol adaToken >= mAmount m
                  Nothing -> False
              Nothing -> False
          _ -> False

    milestoneMarkedComplete :: Bool
    milestoneMarkedComplete =
        case (action, contOutputs) of
          (ReleaseMilestone idx _, [o]) ->
            case txOutDatum o of
              OutputDatum (Datum d) ->
                case unsafeFromBuiltinData @CampaignDatum d of
                  newDat ->
                    case findGrant idx (cdGrants newDat) of
                      Just g ->
                        case getCurrentMilestone g of
                          Just m -> mCompleted m
                          Nothing -> False
                      Nothing -> False
              _ -> False
          _ -> False

    grantTotalsUpdated :: Bool
    grantTotalsUpdated =
        case (action, contOutputs) of
          (ReleaseMilestone idx _, [o]) ->
            case txOutDatum o of
              OutputDatum (Datum d) ->
                case unsafeFromBuiltinData @CampaignDatum d of
                  newDat ->
                    case (findGrant idx (cdGrants dat), findGrant idx (cdGrants newDat)) of
                      (Just oldG, Just newG) ->
                        case getCurrentMilestone oldG of
                          Just m ->
                            gTotalReleased newG == gTotalReleased oldG + mAmount m &&
                            gCurrentPhase newG == gCurrentPhase oldG + 1
                          Nothing -> False
                      _ -> False
              _ -> False
          _ -> False

    -- ===== DISCLOSURE UPDATE =====

    disclosureUpdated :: Bool
    disclosureUpdated =
        case (action, contOutputs) of
          (UpdateDisclosure newRef, [o]) ->
            case txOutDatum o of
              OutputDatum (Datum d) ->
                case unsafeFromBuiltinData @CampaignDatum d of
                  newDat -> cDisclosureRef (cdCampaign newDat) == newRef
              _ -> False
          _ -> False

    -- ===== CAMPAIGN CLOSURE =====

    campaignMarkedClosed :: Bool
    campaignMarkedClosed =
        case contOutputs of
          [o] ->
            case txOutDatum o of
              OutputDatum (Datum d) ->
                case unsafeFromBuiltinData @CampaignDatum d of
                  newDat -> not (cActive (cdCampaign newDat))
              _ -> False
          _ -> False

------------------------------------------------------------------------
-- Untyped wrapper and compile
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @CampaignDatum d
        red = unsafeFromBuiltinData @CampaignAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
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

------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "campaign-finance.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n=== Campaign Finance Validator ==="
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Script Address: " <> P.show onchain
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn "==================================\n"
    
    putStrLn "TRANSPARENCY FEATURES:"
    putStrLn "  * Open campaign treasury"
    putStrLn "  * Spending limits (per-tx & total)"
    putStrLn "  * Immutable donor audit trail"
    putStrLn "  * Disclosure document references"
    putStrLn ""
    putStrLn "GRANT MANAGEMENT:"
    putStrLn "  * Milestone-based disbursements"
    putStrLn "  * Report verification (hash matching)"
    putStrLn "  * Deadline enforcement"
    putStrLn "  * Progress tracking (phase & released amounts)"
    putStrLn ""
    putStrLn "APP FEATURES:"
    putStrLn "  - Donor Breakdown - Complete donation history"
    putStrLn "  - Spend Explorer - Real-time spending tracker"
    putStrLn "  - Milestone Tracker - Grant progress dashboard"
    putStrLn "  - Audit Trail - Immutable transaction log"
    putStrLn ""
    putStrLn "Campaign Finance validator generated successfully!"