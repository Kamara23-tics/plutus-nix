-----

# 🧾 Detailed Tutorial: Understanding and Using `Communication.hs`

This tutorial covers the `Communication.hs` module. This is a multi-functional Plutus V2 smart contract that handles two distinct use cases: **Prepaid Service Vouchers** (for data/voice) and **Anti-Spam Message Escrow** (financial stakes for communication).

-----

## 📚 Table of Contents

1.  [📦 Imports Overview](https://www.google.com/search?q=%231-imports-overview)
2.  [🗃️ Data Structures](https://www.google.com/search?q=%232-data-structures)
3.  [🧠 Core Validator Logic](https://www.google.com/search?q=%233-core-validator-logic)
4.  [⚙️ Validator Script Compilation](https://www.google.com/search?q=%234-validator-script-compilation)
5.  [🔧 Helper Functions](https://www.google.com/search?q=%235-helper-functions)
6.  [🧪 Practical Usage Example](https://www.google.com/search?q=%236-practical-usage-example)
7.  [🧷 Testing Strategy](https://www.google.com/search?q=%237-testing-strategy)
8.  [✅ Best Practices](https://www.google.com/search?q=%238-best-practices)
9.  [📘 Glossary of Terms](https://www.google.com/search?q=%239-glossary-of-terms)

-----

## 1\. 📦 Imports Overview

### Plutus API Modules

  * **Plutus.V2.Ledger.Api & Contexts:**
    Provides the core types (`POSIXTime`, `PubKeyHash`) and context tools (`ScriptContext`, `TxInfo`, `getContinuingOutputs`) required for V2 validation logic.

  * **Plutus.V1.Ledger.Interval:**
    Crucial for the logic in this contract, used to calculate Time-to-Live (TTL) for messages and expiration dates for vouchers.

  * **Plutus.V1.Ledger.Value:**
    Utilities (`valueOf`, `adaSymbol`) used to verify that specific amounts of ADA (fees or deposits) are paid to the correct parties.

### Serialization & Cardano API

  * **Codec.Serialise & Data.ByteString:**
    Essential for converting the compiled validator into bytes that can be written to disk.

  * **Cardano.Api:**
    Used in the `main` function to generate the Bech32 address (e.g., `addr_test1...`) directly within the Haskell script.

-----

## 2\. 🗃️ Data Structures

This contract uses a **Sum Type** for its Datum, allowing one script address to handle two different types of state.

### `CommDatum` (The State)

  * **`VoucherDatum Voucher`**: Holds the state for a prepaid card.
  * **`MessageDatum MsgEscrow`**: Holds the state for a pending message transaction.

### `Voucher`

Represents a user's prepaid balance:

  * `vOwner`: The user who owns the quota.
  * `vIssuer`: The service provider (receives fees).
  * `vQuotaData` / `vQuotaVoice`: Balances for specific services.
  * `vExpiry`: Timestamp when the voucher becomes invalid.

### `MsgEscrow`

Represents a message backed by a financial deposit to prevent spam:

  * `meSender` / `meRecipient`: The parties involved.
  * `meDeposit`: The amount of ADA locked to prove the message is important.
  * `meTTL`: "Time To Live" – if the recipient ignores the message, the sender can reclaim funds after this time.
  * `meAccepted`: Boolean flag tracking if the message was read/accepted.

### `CommAction` (The Redeemer)

The actions dictate which logic path to follow:

  * **Voucher Actions:** `RedeemVoucher`, `TopUpVoucher`.
  * **Message Actions:** `SendMessage`, `AcceptMessage`, `RejectMessage`, `ReclaimExpired`.

-----

## 3\. 🧠 Core Validator Logic

### `mkValidator`

**Purpose:**
This function acts as a router. It checks which type of Datum is present (`Voucher` or `Message`) and which Action is being performed, then enforces specific business rules.

**1. Voucher Logic**

  * **Redeem:** checks `isNotExpired`, ensures `hasEnoughQuota`, and verifies a fee is paid to the `vIssuer`. It enforces state continuity (the output datum must reflect the `deductQuota` calculation).
  * **TopUp:** Requires signatures from **both** the Owner and Issuer. Verifies that the issuer received payment for the new quota and updates the datum using `addQuota`.

**2. Message Escrow Logic (Spam Prevention)**
This implements a game-theoretic approach to messaging:

  * **Send:** Initial lock (implied by script creation). Checks signature.
  * **Accept (Happy Path):** The recipient signs to accept the message. The contract validates that the `meDeposit` is **returned to the Sender**.
  * **Reject (Spam Path):** The recipient signs to reject. The contract validates that the `meDeposit` is **paid to the Recipient** as compensation for spam.
  * **Reclaim (Timeout):** If the recipient ignores the message and `meTTL` has passed, the Sender can withdraw their deposit.

-----

## 4\. ⚙️ Validator Script Compilation

### `mkValidatorUntyped`

Wraps the typed logic into `BuiltinData` for the Plutus Core machine.

### `validator`

Uses Template Haskell (`$$(PlutusTx.compile ...)`) to compile the logic into a `Validator` that can be executed on the Cardano blockchain.

-----

## 5\. 🔧 Helper Functions

### Logic Helpers

  * **`hasEnoughQuota` / `deductQuota`:** Pure functions that handle the math for reducing data/voice balances based on the `ServiceType`.
  * **`isExpired` / `isNotExpired`:** Time utility wrappers around `Interval.contains` to make the validator code more readable.

### IO Helpers

  * **`writeValidator`:** Serializes the compiled code to a `.plutus` file.
  * **`toBech32ScriptAddress`:** Converts the validator hash into a usable Testnet or Mainnet address string.

-----

## 6\. 🧪 Practical Usage Example

The `main` function acts as a build script.

```haskell
-- 1. Compiles the validator
-- 2. Writes it to "communication.plutus"
-- 3. Prints the Script Hash and Testnet Address
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "communication.plutus" validator
    
    -- Outputs details about the features (Spam prevention, Top-up store, etc.)
    putStrLn "PREPAID VOUCHERS: ..."
```

**To Run:**
Execute `cabal run` in your terminal. This generates the file needed to build transactions via Cardano CLI.

-----

## 7\. 🧷 Testing Strategy

  * **Quota Boundary:** Test redeeming exactly the remaining amount of Data vs 1 MB more (should fail).
  * **Fee Verification:** Ensure the transaction fails if the Issuer is paid 1 Lovelace less than required during a TopUp.
  * **TTL Boundary:**
      * Try to `ReclaimExpired` 1 second *before* the TTL (should fail).
      * Try to `AcceptMessage` 1 second *after* the TTL (should fail).
  * **Deposit Routing:** Verify that `RejectMessage` actually moves the ADA to the recipient, not the sender.

-----

## 8\. ✅ Best Practices

  * **Datum Continuity:** In the `VoucherUpdated` check, the contract strictly matches the input datum to the output datum. This prevents a user from maliciously changing the "Issuer" field or "Expiry" field while redeeming quota.
  * **Double Satisfaction:** The contract checks `getContinuingOutputs` to ensure the state is carried forward correctly, preventing the "Double Satisfaction" vulnerability.
  * **Lovelace Precision:** All fee calculations (`serviceFee`, `dataPrice`) are done in Integers (Lovelace). Always remember 1 ADA = 1,000,000 Lovelace.

-----

## 9\. 📘 Glossary of Terms

| Term | Definition |
| :--- | :--- |
| **Voucher** | A record of prepaid Data (MB) and Voice (Minutes) credits held on-chain. |
| **Escrow** | A financial arrangement where a third party (the contract) holds funds (the deposit) until conditions are met. |
| **TTL (Time-to-Live)** | A duration or timestamp after which a message or transaction is considered expired. |
| **ServiceType** | An enumeration (`DataService` or `VoiceService`) used to determine which quota to deduct. |
| **Issuer** | The service provider entity authorized to receive fees and sign off on Top-ups. |
| **Spam Deposit** | A financial stake attached to a message. It is returned if the message is valid, or forfeited if it is spam. |
| **Continuing Output** | A transaction output that sends value back to the same script address (used to update state). |