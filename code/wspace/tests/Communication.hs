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

-- | Service type for vouchers
data ServiceType = DataService | VoiceService | BothServices
PlutusTx.unstableMakeIsData ''ServiceType

-- | Prepaid voucher for communication services
data Voucher = Voucher
    { vOwner      :: PubKeyHash           -- Voucher owner
    , vQuotaData  :: Integer              -- Data quota in MB
    , vQuotaVoice :: Integer              -- Voice quota in minutes
    , vExpiry     :: POSIXTime            -- Expiration timestamp
    , vIssuer     :: PubKeyHash           -- Service provider who issued
    }
PlutusTx.unstableMakeIsData ''Voucher

-- | Message escrow with spam prevention deposit
data MsgEscrow = MsgEscrow
    { meSender     :: PubKeyHash          -- Message sender
    , meRecipient  :: PubKeyHash          -- Intended recipient
    , meDeposit    :: Integer             -- Deposit amount (lovelace)
    , meMsgHash    :: BuiltinByteString   -- Hash of encrypted message content
    , meTTL        :: POSIXTime           -- Time-to-live / expiry
    , meAccepted   :: Bool                -- Whether recipient accepted
    }
PlutusTx.unstableMakeIsData ''MsgEscrow

-- | Combined datum for the contract
data CommDatum
    = VoucherDatum Voucher                -- Prepaid voucher
    | MessageDatum MsgEscrow              -- Message with deposit
PlutusTx.unstableMakeIsData ''CommDatum

-- | Redeemer actions
data CommAction
    = RedeemVoucher ServiceType Integer   -- Redeem voucher (service type, amount)
    | TopUpVoucher Integer Integer        -- Top up voucher (data MB, voice mins)
    | SendMessage                         -- Send message (creates escrow)
    | AcceptMessage                       -- Recipient accepts message
    | RejectMessage                       -- Recipient rejects message
    | ReclaimExpired                      -- Sender reclaims after TTL expires
PlutusTx.unstableMakeIsData ''CommAction

------------------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------------------

{-# INLINABLE isExpired #-}
-- | Check if a timestamp has passed
isExpired :: POSIXTime -> POSIXTimeRange -> Bool
isExpired deadline txRange = 
    Interval.contains (Interval.from (deadline + 1)) txRange

{-# INLINABLE isNotExpired #-}
-- | Check if timestamp hasn't passed yet
isNotExpired :: POSIXTime -> POSIXTimeRange -> Bool
isNotExpired deadline txRange =
    Interval.contains (Interval.to deadline) txRange

{-# INLINABLE hasEnoughQuota #-}
-- | Check if voucher has sufficient quota
hasEnoughQuota :: Voucher -> ServiceType -> Integer -> Bool
hasEnoughQuota v stype amount =
    case stype of
      DataService  -> vQuotaData v >= amount
      VoiceService -> vQuotaVoice v >= amount
      BothServices -> False  -- Not allowed for single redemption

{-# INLINABLE deductQuota #-}
-- | Deduct quota from voucher
deductQuota :: Voucher -> ServiceType -> Integer -> Voucher
deductQuota v stype amount =
    case stype of
      DataService  -> v { vQuotaData = vQuotaData v - amount }
      VoiceService -> v { vQuotaVoice = vQuotaVoice v - amount }
      BothServices -> v  -- No deduction for invalid case

{-# INLINABLE addQuota #-}
-- | Add quota to voucher
addQuota :: Voucher -> Integer -> Integer -> Voucher
addQuota v dataAmount voiceAmount = v 
    { vQuotaData = vQuotaData v + dataAmount
    , vQuotaVoice = vQuotaVoice v + voiceAmount
    }

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: CommDatum -> CommAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case (dat, action) of
      -- Redeem voucher for service
      (VoucherDatum voucher, RedeemVoucher stype amount) ->
         traceIfFalse "Owner signature missing" ownerSigned &&
         traceIfFalse "Voucher expired" voucherNotExpired &&
         traceIfFalse "Insufficient quota" sufficientQuota &&
         traceIfFalse "Issuer not paid service fee" issuerPaid &&
         traceIfFalse "Voucher not updated correctly" voucherUpdated

      -- Top up existing voucher
      (VoucherDatum voucher, TopUpVoucher dataAmt voiceAmt) ->
         traceIfFalse "Owner signature missing" ownerSigned &&
         traceIfFalse "Issuer signature missing" issuerSigned &&
         traceIfFalse "Payment not received" topUpPaymentReceived &&
         traceIfFalse "Voucher not topped up correctly" voucherToppedUp

      -- Send message with deposit
      (MessageDatum msgEscrow, SendMessage) ->
         traceIfFalse "Sender signature missing" senderSigned &&
         traceIfFalse "Deposit not locked" depositLocked &&
         traceIfFalse "Message not within TTL" withinTTL

      -- Recipient accepts message (deposit returned)
      (MessageDatum msgEscrow, AcceptMessage) ->
         traceIfFalse "Recipient signature missing" recipientSigned &&
         traceIfFalse "Message expired" messageNotExpired &&
         traceIfFalse "Message already accepted" notYetAccepted &&
         traceIfFalse "Sender not refunded deposit" senderRefunded &&
         traceIfFalse "Acceptance not recorded" acceptanceRecorded

      -- Recipient rejects message (deposit forfeited)
      (MessageDatum msgEscrow, RejectMessage) ->
         traceIfFalse "Recipient signature missing" recipientSigned &&
         traceIfFalse "Message expired" messageNotExpired &&
         traceIfFalse "Deposit not sent to recipient" depositToRecipient

      -- Sender reclaims after TTL expires (no response)
      (MessageDatum msgEscrow, ReclaimExpired) ->
         traceIfFalse "Sender signature missing" senderSigned &&
         traceIfFalse "TTL not expired yet" ttlExpired &&
         traceIfFalse "Deposit not returned to sender" depositReturned

      _ -> traceIfFalse "Invalid datum/redeemer combination" False

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    -- Get continuing outputs
    contOutputs :: [TxOut]
    contOutputs = getContinuingOutputs ctx

    -- ===== VOUCHER VALIDATION =====

    ownerSigned :: Bool
    ownerSigned =
        case dat of
          VoucherDatum v -> txSignedBy info (vOwner v)
          _ -> False

    issuerSigned :: Bool
    issuerSigned =
        case dat of
          VoucherDatum v -> txSignedBy info (vIssuer v)
          _ -> False

    voucherNotExpired :: Bool
    voucherNotExpired =
        case dat of
          VoucherDatum v -> isNotExpired (vExpiry v) txRange
          _ -> False

    sufficientQuota :: Bool
    sufficientQuota =
        case (dat, action) of
          (VoucherDatum v, RedeemVoucher stype amount) -> 
              hasEnoughQuota v stype amount
          _ -> False

    issuerPaid :: Bool
    issuerPaid =
        case (dat, action) of
          (VoucherDatum v, RedeemVoucher _ amount) ->
            -- Small service fee (1 ADA per 100 units as example)
            let serviceFee = (amount * 1000000) `divide` 100  -- lovelace
                issuerValue = valuePaidTo info (vIssuer v)
            in valueOf issuerValue adaSymbol adaToken >= serviceFee
          _ -> False

    voucherUpdated :: Bool
    voucherUpdated =
        case (dat, action, contOutputs) of
          (VoucherDatum oldV, RedeemVoucher stype amount, [outTxOut]) ->
            case txOutDatum outTxOut of
              OutputDatum (Datum d) ->
                case unsafeFromBuiltinData @CommDatum d of
                  VoucherDatum newV ->
                    let expected = deductQuota oldV stype amount
                    in vQuotaData newV == vQuotaData expected &&
                       vQuotaVoice newV == vQuotaVoice expected
                  _ -> False
              _ -> False
          _ -> False

    topUpPaymentReceived :: Bool
    topUpPaymentReceived =
        case (dat, action) of
          (VoucherDatum v, TopUpVoucher dataAmt voiceAmt) ->
            -- Payment calculation (example: 1 ADA per 10 data units, 2 ADA per voice min)
            let dataPrice = (dataAmt * 1000000) `divide` 10
                voicePrice = voiceAmt * 2000000
                totalPrice = dataPrice + voicePrice
                issuerValue = valuePaidTo info (vIssuer v)
            in valueOf issuerValue adaSymbol adaToken >= totalPrice
          _ -> False

    voucherToppedUp :: Bool
    voucherToppedUp =
        case (dat, action, contOutputs) of
          (VoucherDatum oldV, TopUpVoucher dataAmt voiceAmt, [outTxOut]) ->
            case txOutDatum outTxOut of
              OutputDatum (Datum d) ->
                case unsafeFromBuiltinData @CommDatum d of
                  VoucherDatum newV ->
                    let expected = addQuota oldV dataAmt voiceAmt
                    in vQuotaData newV == vQuotaData expected &&
                       vQuotaVoice newV == vQuotaVoice expected
                  _ -> False
              _ -> False
          _ -> False

    -- ===== MESSAGE ESCROW VALIDATION =====

    senderSigned :: Bool
    senderSigned =
        case dat of
          MessageDatum me -> txSignedBy info (meSender me)
          _ -> False

    recipientSigned :: Bool
    recipientSigned =
        case dat of
          MessageDatum me -> txSignedBy info (meRecipient me)
          _ -> False

    depositLocked :: Bool
    depositLocked =
        case dat of
          MessageDatum me ->
            case contOutputs of
              [_] -> True  -- Deposit continues in contract
              _ -> False
          _ -> False

    withinTTL :: Bool
    withinTTL =
        case dat of
          MessageDatum me -> isNotExpired (meTTL me) txRange
          _ -> False

    messageNotExpired :: Bool
    messageNotExpired = withinTTL

    ttlExpired :: Bool
    ttlExpired =
        case dat of
          MessageDatum me -> isExpired (meTTL me) txRange
          _ -> False

    notYetAccepted :: Bool
    notYetAccepted =
        case dat of
          MessageDatum me -> not (meAccepted me)
          _ -> False

    senderRefunded :: Bool
    senderRefunded =
        case dat of
          MessageDatum me ->
            let senderValue = valuePaidTo info (meSender me)
            in valueOf senderValue adaSymbol adaToken >= meDeposit me
          _ -> False

    acceptanceRecorded :: Bool
    acceptanceRecorded =
        case (dat, contOutputs) of
          (MessageDatum oldMe, [outTxOut]) ->
            case txOutDatum outTxOut of
              OutputDatum (Datum d) ->
                case unsafeFromBuiltinData @CommDatum d of
                  MessageDatum newMe -> meAccepted newMe
                  _ -> False
              _ -> False
          _ -> False

    depositToRecipient :: Bool
    depositToRecipient =
        case dat of
          MessageDatum me ->
            let recipientValue = valuePaidTo info (meRecipient me)
            in valueOf recipientValue adaSymbol adaToken >= meDeposit me
          _ -> False

    depositReturned :: Bool
    depositReturned = senderRefunded

------------------------------------------------------------------------
-- Untyped wrapper and compile
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @CommDatum d
        red = unsafeFromBuiltinData @CommAction r
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

    writeValidator "communication.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Communication Voucher & Spam Prevention Validator ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "--------------------------------------------------------"
    putStrLn ""
    putStrLn "PREPAID VOUCHERS:"
    putStrLn "  - Data quota (MB) and Voice quota (minutes)"
    putStrLn "  - Expiration timestamps"
    putStrLn "  - Redeemable for service with issuer payment"
    putStrLn "  - Top-up functionality for existing vouchers"
    putStrLn ""
    putStrLn "SPAM PREVENTION:"
    putStrLn "  - Messages require deposit escrow"
    putStrLn "  - Deposit refunded when recipient accepts"
    putStrLn "  - Deposit forfeited if recipient rejects"
    putStrLn "  - Auto-refund after TTL expiry (no response)"
    putStrLn ""
    putStrLn "APP FEATURES:"
    putStrLn "  - Top-up store for voucher purchases"
    putStrLn "  - Inbox with deposit amount filters"
    putStrLn "  - Accept/Reject message interface"
    putStrLn "  - Service usage tracking"
    putStrLn ""
    putStrLn "Communication validator generated successfully."