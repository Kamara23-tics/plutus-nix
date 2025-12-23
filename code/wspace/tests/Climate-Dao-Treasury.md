---

# 🌳 Detailed Tutorial: Understanding and Using `Climate DAO Treasury` Smart Contract

This tutorial covers the **Climate DAO Treasury** smart contract, highlighting its purpose, critical imports, datum and redeemer structures, validator logic, and practical usage scenarios.
This contract enables **proposal-based treasury funding** with strict time and signature constraints using **Plutus V2**.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Datum and Redeemer](#2-datum-and-redeemer)
3. [🔧 Helper Functions](#3-helper-functions)
4. [🧠 Core Validator Logic](#4-core-validator-logic)
5. [⚙️ Validator Script Compilation](#5-validator-script-compilation)
6. [🏠 Validator Hash and Addresses](#6-validator-hash-and-addresses)
7. [🧪 Practical Usage Example](#7-practical-usage-example)
8. [🧷 Testing Strategy](#8-testing-strategy)
9. [✅ Best Practices](#9-best-practices)
10. [📘 Glossary of Terms](#10-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus API Modules

* **Plutus.V2.Ledger.Api**
  Provides core on-chain types such as `Validator`, `POSIXTime`, `PubKeyHash`, `ScriptContext`, and `TxInfo`.

* **Plutus.V2.Ledger.Contexts**
  Supplies transaction inspection utilities including:

  * `txSignedBy`
  * `valuePaidTo`
  * `txInfoValidRange`
  * `findOwnInput`

* **Plutus.V1.Ledger.Interval**
  Used for time validation through interval functions like `contains`, `from`, and `to`.

* **Plutus.V1.Ledger.Value**
  Enables extraction and comparison of ADA values using `valueOf`, `adaSymbol`, and `adaToken`.

---

### Utility, Serialization, and Prelude Modules

* **PlutusTx**
  Enables Template Haskell compilation and on-chain data serialization.

* **PlutusTx.Prelude**
  A restricted Prelude suitable for deterministic on-chain execution.

* **NumericUnderscores**
  Improves readability of large integer values (e.g. `3_000_000` Lovelace).

* **Codec.Serialise & ByteString modules**
  Used to serialize the validator into CBOR format.

* **Cardano.Api / Cardano.Api.Shelley**
  Used off-chain to derive Bech32 script addresses for CLI usage.

---

## 2. 🗃️ Datum and Redeemer

### `ProposalDatum`

Represents the on-chain state of a single DAO funding proposal.

Fields include:

* `pdProposer`
  The public key hash of the entity submitting the proposal.

* `pdFundingGoal`
  The minimum required funding amount in Lovelace.

* `pdVotingDeadline`
  A POSIX timestamp defining when voting ends.

* `pdRecipient`
  The public key hash of the entity that will receive funds if the proposal is funded.

This datum is serialized on-chain using `unstableMakeIsData`.

---

### `TreasuryAction`

Defines the actions that may be performed on the treasury UTXO:

* `FundProposal`
  Releases funds to the recipient after the voting deadline.

* `CancelProposal`
  Cancels the proposal and refunds funds to the proposer before the deadline.

---

## 3. 🔧 Helper Functions

### `scriptInputContainsAda`

Checks that the script input UTXO contains **at least the required funding goal in ADA**.

Function behavior:

* Locates the script input using `findOwnInput`
* Extracts its value
* Verifies the ADA amount meets or exceeds the funding goal
* Throws a trace error if the script input is missing

---

## 4. 🧠 Core Validator Logic

### `mkValidator`

The validator enforces **two mutually exclusive execution paths**, based on the redeemer.

---

### 🔵 `FundProposal`

A proposal can be funded only if **all** of the following conditions are met:

* The transaction is signed by the proposer
* The voting deadline has passed
* The script UTXO contains sufficient ADA
* The recipient receives at least the funding goal

---

### 🔴 `CancelProposal`

A proposal can be canceled only if **all** of the following conditions are met:

* The transaction is signed by the proposer
* The voting deadline has **not** passed
* The proposer receives a refund of at least the funding goal

---

### Time Enforcement

Time conditions are enforced using:

* `POSIXTimeRange`
* `txInfoValidRange`
* `Interval.contains`

This ensures correct behavior at the boundary between voting and execution periods.

---

## 5. ⚙️ Validator Script Compilation

### `mkValidatorUntyped`

Wraps the typed validator to make it compatible with Plutus Core by:

* Decoding datum, redeemer, and context from `BuiltinData`
* Returning `()` on success
* Throwing an error on validation failure

---

### `validator`

The final on-chain validator is compiled using Template Haskell:

```haskell
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])
```

This produces a **Plutus V2 validator script**.

---

## 6. 🏠 Validator Hash and Addresses

The contract derives addresses in two ways:

* **Validator Hash**
  Computed from the serialized Plutus script.

* **On-chain Script Address**
  Built using a `ScriptCredential`.

* **Bech32 Address**
  Generated via `cardano-api` for use with `cardano-cli`.

These addresses allow interaction with the contract both on-chain and off-chain.

---

## 7. 🧪 Practical Usage Example

```haskell
-- Write the compiled validator to file
writeValidator "climate-dao-treasury.plutus" validator

-- Print Bech32 address for testnet
toBech32ScriptAddress (Testnet (NetworkMagic 1)) validator
```

Typical workflow:

1. Deploy the validator script
2. Lock funds at the script address with a `ProposalDatum`
3. Spend using either `FundProposal` or `CancelProposal` redeemer

---

## 8. 🧷 Testing Strategy

* Test funding attempts before and after the voting deadline
* Validate correct signature enforcement
* Ensure refunds fail after the deadline
* Verify insufficient ADA causes validation failure
* Test boundary conditions around time intervals

---

## 9. ✅ Best Practices

* Always validate **signatures, time, and value flow**
* Use clear `traceIfFalse` messages for debugging
* Test deadline edge cases thoroughly
* Keep governance logic explicit and deterministic

---

## 10. 📘 Glossary of Terms

| Term           | Definition                                            |
| -------------- | ----------------------------------------------------- |
| **DAO**        | Decentralized Autonomous Organization                 |
| **Proposal**   | A request for funding from the treasury               |
| **Datum**      | On-chain state associated with a script UTXO          |
| **Redeemer**   | Specifies how a script UTXO is spent                  |
| **Validator**  | On-chain logic that approves or rejects transactions  |
| **POSIXTime**  | Time representation used in Plutus                    |
| **PubKeyHash** | Hash of a public key used for authorization           |
| **txSignedBy** | Checks whether a transaction is signed by a given key |
| **Bech32**     | Human-readable Cardano address format                 |
| **Plutus V2**  | Second version of the Plutus smart contract language  |

---


