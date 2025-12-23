---

# 🧾 Detailed Tutorial: Understanding and Using `PGEF Staking Vault`

This tutorial covers the **PGEF Staking Vault** smart contract, highlighting its purpose, critical imports, validator logic, and practical usage scenarios. This module implements a simple on-chain **staking and yield accrual mechanism** using **Plutus V2**.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Data Structures](#2-data-structures)
3. [🔧 Helper Functions](#3-helper-functions)
4. [🧠 Core Validator Logic](#4-core-validator-logic)
5. [⚙️ Validator Script Compilation](#5-validator-script-compilation)
6. [🛠️ Utility Functions](#6-utility-functions)
7. [🧪 Practical Usage Example](#7-practical-usage-example)
8. [🧷 Testing Strategy](#8-testing-strategy)
9. [✅ Best Practices](#9-best-practices)
10. [📘 Glossary of Terms](#10-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus API Modules

* **Plutus.V2.Ledger.Api**
  Provides fundamental on-chain types such as `POSIXTime`, `ScriptContext`,
  `Validator`, `PubKeyHash`, and `BuiltinData`.

* **Plutus.V2.Ledger.Contexts**
  Supplies helper functions for inspecting transactions, including
  `txSignedBy`, `valuePaidTo`, and `scriptContextTxInfo`.

* **Plutus.V1.Ledger.Interval**
  Used for extracting and validating time ranges from a transaction’s
  validity interval.

* **Plutus.V1.Ledger.Value**
  Enables inspection of ADA values using `valueOf`, `adaSymbol`, and `adaToken`.

### Utility, Serialization, and Cardano API

* **PlutusTx & PlutusTx.Prelude**
  Enable on-chain compilation, data encoding, and safe integer operations.

* **Codec.Serialise & ByteString modules**
  Used to serialize the compiled validator.

* **Cardano.Api & Cardano.Api.Shelley**
  Used to generate Bech32 script addresses and write the validator in standard
  `.plutus` JSON (TextEnvelope) format.

---

## 2. 🗃️ Data Structures

### `StakingDatum`

Defines the on-chain state of a staking position:

* `sdStaker`
  The public key hash of the staker.

* `sdPrincipal`
  The amount of ADA initially staked.

* `sdStartTime`
  The POSIX timestamp when staking began.

* `sdClaimedYield`
  Yield already withdrawn by the staker.

* `sdActive`
  Indicates whether the stake is still active.

This datum fully describes a single staking position stored at the script
address.

---

### `StakingAction`

Defines the actions that can be performed on the staking UTXO:

* `Stake`
  Validates the staking position when it is created or continued.

* `ClaimAndUnstake`
  Allows the staker to claim accrued yield and withdraw their principal.

---

## 3. 🔧 Helper Functions

### Time Handling

* **`unPosixTimeHelper`**
  Extracts the raw integer value from a `POSIXTime`.

* **`getCurrentTime`**
  Determines the current on-chain time by reading the upper bound of the
  transaction’s validity interval.

---

### Yield Calculation

* **`calculateAccruedYield`**
  Computes staking rewards based on:

  * Time elapsed since `sdStartTime`
  * Fixed annual yield rate (5%)
  * Staked principal

The calculation uses integer arithmetic only, avoiding floating-point
operations to ensure deterministic on-chain behavior.

---

## 4. 🧠 Core Validator Logic

### `mkValidator`

The validator enforces rules based on the supplied `StakingAction`.

---

#### **Stake**

The transaction is valid only if:

* It is signed by the staker (`txSignedBy`)
* The stake is marked as active
* The principal amount is greater than zero

---

#### **ClaimAndUnstake**

The transaction is valid only if:

* It is signed by the staker
* The stake is still active
* The staker is paid at least:

  ```
  principal + accruedYield − claimedYield
  ```

This ensures that the staker receives the correct payout before the stake is
closed.

---

## 5. ⚙️ Validator Script Compilation

### Untyped Wrapper

The function `mkValidatorUntyped` converts typed datum and redeemer values
from `BuiltinData`, making the validator compatible with on-chain execution.

### `validator`

The validator is compiled into Plutus Core using Template Haskell and wrapped
as a `Validator` ready for deployment.

---

## 6. 🛠️ Utility Functions

* **`plutusValidatorHash`**
  Computes the hash of the compiled validator.

* **`plutusScriptAddress`**
  Derives the on-chain script address from the validator hash.

* **`toBech32ScriptAddress`**
  Produces a Bech32-encoded address for use with `cardano-cli`.

* **`writeValidator`**
  Writes the validator to a `.plutus` file using the Cardano API
  TextEnvelope format.

---

## 7. 🧪 Practical Usage Example

```haskell
-- Compile and write the validator
cabal run

-- Output
pgef-staking-vault.plutus

-- Printed to terminal
Validator Hash (Plutus)
Bech32 Script Address (Testnet)
```

This output can be used directly with `cardano-cli` or off-chain tooling.

---

## 8. 🧷 Testing Strategy

* Test staking with zero and non-zero principals
* Verify yield increases over time
* Test partial and incorrect payouts
* Ensure invalid signatures fail validation
* Check behavior when `sdActive` is false

---

## 9. ✅ Best Practices

* Use integer arithmetic for all financial logic
* Explicitly validate value transfers
* Provide clear `traceIfFalse` error messages
* Test boundary cases for time-based logic
* Keep datum fields minimal but complete

---

## 10. 📘 Glossary of Terms

| Term             | Definition                                      |
| ---------------- | ----------------------------------------------- |
| **Staking**      | Locking ADA in a script to earn yield over time |
| **Datum**        | On-chain data that determines script behavior   |
| **Validator**    | A script that enforces spending conditions      |
| **POSIXTime**    | Time representation used in Plutus              |
| **Yield**        | Reward earned based on staking duration         |
| **PubKeyHash**   | Hash of a public key used for authorization     |
| **Bech32**       | Human-readable Cardano address format           |
| **BuiltinData**  | Untyped data format used on-chain               |
| **TextEnvelope** | Standard JSON format for `.plutus` scripts      |
| **Testnet**      | Cardano testing environment                     |

---
