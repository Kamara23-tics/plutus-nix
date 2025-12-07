Here is your detailed tutorial for the `EscrowNFT.hs` smart contract, reformatted to match the structure of your `Vesting.hs` example.

-----

# 🧾 Detailed Tutorial: Understanding and Using `EscrowNFT.hs`

This tutorial covers the `EscrowNFT.hs` module. This module implements a trustless exchange mechanism (Swap) for an NFT, ensuring that a buyer receives the token if and only if the seller receives the agreed-upon ADA amount. It also includes a time-lock mechanism allowing the seller to reclaim the item if the trade does not occur by the deadline.

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

  * **Plutus.V2.Ledger.Api:**
    Provides fundamental types such as `POSIXTime`, `PubKeyHash`, `CurrencySymbol`, `TokenName`, and transaction context (`ScriptContext`, `TxInfo`).

  * **Plutus.V2.Ledger.Contexts:**
    Contains utility functions for transaction context validation (`txSignedBy`, `findOwnInput`, `valuePaidTo`).

  * **Plutus.V1.Ledger.Interval:**
    Supplies the `contains` function for checking if the transaction time falls after the deadline.

  * **Plutus.V1.Ledger.Value:**
    Utilities to handle native assets and ADA values (`valueOf`, `adaSymbol`, `adaToken`).

### Utility and Serialization

  * **PlutusTx:**
    Enables script compilation (`compile`) and data serialization (`unstableMakeIsData`).

  * **Codec.Serialise & Data.ByteString:**
    Essential for converting the compiled validator into bytes that can be written to disk.

  * **Cardano.Api:**
    Used in the `main` function to generate the Bech32 address (e.g., `addr_test1...`) directly within the Haskell script, facilitating easier deployment.

-----

## 2\. 🗃️ Data Structures

### `EscrowDatum`

Defines the state of the escrow agreement stored on the blockchain:

  * `edBuyer`: The Public Key Hash of the intended buyer.
  * `edSeller`: The Public Key Hash of the current owner (seller).
  * `edAmount`: The price of the NFT in Lovelace.
  * `edDeadline`: The timestamp after which the seller can cancel the trade.
  * `edCurrency` & `edToken`: The unique identifiers (Policy ID and Token Name) of the NFT being sold.

### `EscrowAction` (Redeemer)

Defines the two possible paths for unlocking the script:

  * `PaySeller`: The successful trade path. The buyer pays the seller and takes the NFT.
  * `RefundSeller`: The cancellation path. The seller reclaims their NFT after the deadline.

-----

## 3\. 🧠 Core Validator Logic

### `mkValidator`

**Purpose:**

This function acts as the arbitrator of the trade. It inspects the transaction context (`ctx`) and the requested action (`EscrowAction`) to determine if the spending is authorized.

**Validation Flows:**

1.  **PaySeller (The Swap):**

      * **NFT Presence:** Verifies the NFT is actually currently locked in the script input (using `scriptInputContainsNFT`).
      * **Authorization:** Must be signed by the `edBuyer`.
      * **Payment:** Checks that `edSeller` receives at least `edAmount` of ADA.
      * **Delivery:** Checks that `edBuyer` receives the specific NFT defined by `edCurrency` and `edToken`.

2.  **RefundSeller (The Cancel):**

      * **NFT Presence:** Verifies the NFT is in the script input.
      * **Authorization:** Must be signed by the `edSeller`.
      * **Timing:** Verifies the transaction is occurring strictly *after* the `edDeadline`.
      * **Reclaim:** Checks that the `edSeller` receives the NFT back.

-----

## 4\. ⚙️ Validator Script Compilation

### `mkValidatorUntyped`

Wraps the core validator function. It converts the raw `BuiltinData` from the chain into the typed `EscrowDatum` and `EscrowAction` before executing the logic.

### `validator`

Uses Template Haskell (`$$(PlutusTx.compile ...)`) to compile the Haskell logic into a Plutus Core script (`PlutusV2.Validator`) ready for the network.

-----

## 5\. 🔧 Helper Functions

### `scriptInputContainsNFT`

A crucial security helper. It looks at the input coming *from* this script (`findOwnInput`) and verifies that the value inside it actually contains the NFT. This prevents someone from spending a UTXO that belongs to the script but doesn't hold the asset (if multiple UTXOs exist at the address).

### `plutusValidatorHash` & `plutusScriptAddress`

Computes the unique hash of your validator logic and derives the Plutus address.

### `toBech32ScriptAddress`

Uses the `Cardano.Api` to convert the raw Plutus address into a human-readable string (Bech32) for the Testnet.

-----

## 6\. 🧪 Practical Usage Example

The `main` function serves as your build and deployment tool:

```haskell
-- 1. Compiles the validator
-- 2. Writes it to "validator.plutus"
-- 3. Prints the Script Hash and Testnet Address
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "validator.plutus" validator

    -- Output useful info for the CLI
    putStrLn "\n--- Escrow NFT Validator Info ---"
    -- ... prints hash and address ...
```

-----

## 7\. 🧷 Testing Strategy

To ensure this contract is secure, your tests should cover:

  * **The "Happy Path":** Buyer signs, Seller gets paid, Buyer gets NFT.
  * **The "Refund Path":** Seller signs, Time \> Deadline, Seller gets NFT.
  * **Theft Attempts:**
      * Buyer tries to take NFT without paying enough ADA.
      * Buyer tries to take NFT *and* keep the ADA.
      * Seller tries to reclaim the NFT *before* the deadline.
  * **Token Spoofing:** Ensure the validator checks for the *exact* CurrencySymbol and TokenName, not just any token.

-----

## 8\. ✅ Best Practices

  * **Explicit Asset Checking:** The `scriptInputContainsNFT` helper is a great practice. It ensures the logic applies to the specific UTXO holding the asset, preventing attacks involving other UTXOs at the same script address.
  * **Double Satisfaction:** By explicitly checking `valuePaidTo` for both the Seller (ADA) and the Buyer (NFT), you prevent "Double Satisfaction" attacks where one payment might try to satisfy two different script requirements simultaneously.
  * **Datum Integrity:** Ensure that the `EscrowDatum` is correctly constructed off-chain with the correct Policy ID and Token Name. If these are wrong in the datum, the NFT might become permanently locked.

-----

## 9\. 📘 Glossary of Terms

| Term | Definition |
| :--- | :--- |
| **Escrow** | A financial arrangement where a third party (the contract) holds an asset until specific conditions (payment) are met. |
| **NFT** | Non-Fungible Token. A unique digital asset identified by a Policy ID and Token Name. |
| **CurrencySymbol** | The hash of the minting policy (Policy ID) that acts as the "family" ID for the token. |
| **TokenName** | The specific name of the token within that policy (e.g., "MonaLisa"). |
| **Lovelace** | The atomic unit of ADA ($1 \text{ ADA} = 1,000,000 \text{ Lovelace}$). |
| **UTXO** | Unspent Transaction Output. The actual "box" on the blockchain that holds the ADA and NFT. |
| **Bech32** | The encoding format used for Cardano addresses (e.g., starts with `addr_test1...`). |
| **POSIXTime** | A timestamp representation (seconds/milliseconds since 1970) used for the deadline logic. |