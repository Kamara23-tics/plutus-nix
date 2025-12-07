-----

# 🧾 Detailed Tutorial: Understanding and Using `CampaignFinance.hs`

This tutorial covers the `CampaignFinance.hs` module, a robust Plutus V2 smart contract designed for transparent campaign treasury management. It features distinct logic for donation tracking, operational spending, and milestone-based grant disbursements.

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
    Used for time-based logic, specifically checking if a transaction falls within a milestone's deadline using `contains`.

  * **Plutus.V1.Ledger.Value:**
    Utilities to handle `Value`, `adaSymbol`, and `adaToken` for checking ADA amounts in disbursements.

### Serialization & Cardano API

  * **Codec.Serialise & Data.ByteString:**
    Essential for converting the compiled validator into bytes that can be written to disk.

  * **Cardano.Api:**
    Used in the `main` function to generate the Bech32 address (e.g., `addr_test1...`) directly within the Haskell script, facilitating easier deployment.

-----

## 2\. 🗃️ Data Structures

This contract uses a nested data structure to maintain the entire state of the campaign within the UTXO datum.

### `Milestone`

Defines a specific phase within a grant:

  * `mAmount`: ADA to be released.
  * `mReportHash`: A hash of the off-chain report required to unlock funds (ensures accountability).
  * `mDeadline`: The date by which this milestone must be claimed.
  * `mCompleted`: Boolean flag tracking status.

### `Grant`

Represents a sub-allocation of funds to a recipient:

  * `gMilestones`: A list of `Milestone` objects.
  * `gCurrentPhase`: Integer tracking which milestone is currently active.
  * `gTotalReleased`: Cumulative amount paid out to this recipient.

### `DonorRecord` & `Campaign`

  * **DonorRecord:** An immutable log entry containing the donor's ID, amount, and timestamp.
  * **Campaign:** Holds global configuration (Treasurer ID, spending limits, IPFS disclosure references) and running totals.

### `CampaignDatum` (The State)

The master object stored on-chain:

```haskell
data CampaignDatum = CampaignDatum
    { cdCampaign :: Campaign       -- Config and totals
    , cdGrants   :: [Grant]        -- Active grants
    , cdDonors   :: [DonorRecord]  -- Audit trail
    }
```

### `CampaignAction` (The Redeemer)

The actions a user can perform:

  * **Donate:** Public action to add funds.
  * **DisburseOperational:** Treasurer spends funds on overhead.
  * **CreateGrant:** Treasurer allocates funds for a future project.
  * **ReleaseMilestone:** Unlocks funds for a specific grant phase.
  * **UpdateDisclosure:** Changes the reference to off-chain docs.
  * **CloseCampaign:** Ends the campaign.

-----

## 3\. 🧠 Core Validator Logic

### `mkValidator`

**Purpose:**
This function is a state machine that transitions the `CampaignDatum` based on the `CampaignAction` provided. It enforces strict transparency and authorization rules.

**Key Validation Flows:**

1.  **Donation Logic (`Donate`):**

      * Verifies that the new datum includes exactly one new `DonorRecord`.
      * Ensures `cTotalDonations` is updated correctly.
      * *Transparency:* Prevents "anonymous" pool flooding by forcing a record.

2.  **Operational Spending (`DisburseOperational`):**

      * **Authorization:** Must be signed by the `cTreasurer`.
      * **Limits:** Checks `cPerTxLimit` and `cTotalLimit` to prevent draining the treasury.
      * **Budgeting:** Updates `cTotalSpent` in the datum.

3.  **Grant Management (`CreateGrant` & `ReleaseMilestone`):**

      * **Creation:** Checks if the new grant fits within the global budget.
      * **Release:**
          * Verifies the `mReportHash` provided in the transaction matches the hash stored in the milestone.
          * Checks `milestonePastDeadline`.
          * Ensures the recipient actually receives the funds (`valuePaidTo`).
          * Updates the grant's `gCurrentPhase` and `mCompleted` status.

4.  **Admin Functions:**

      * `UpdateDisclosure` and `CloseCampaign` are strictly restricted to the Treasurer's signature.

-----

## 4\. ⚙️ Validator Script Compilation

### `mkValidatorUntyped`

Wraps the typed logic into `BuiltinData` for the Plutus Core machine.

### `validator`

Uses Template Haskell (`$$(PlutusTx.compile ...)`) to compile the logic into a `Validator` that can be executed on the Cardano blockchain.

-----

## 5\. 🔧 Helper Functions

### Logic Helpers

  * **`findGrant` / `updateGrantAt`:** Utilities to locate and modify a specific grant within the list without breaking other data.
  * **`milestonePastDeadline`:** Uses `Interval.contains` to ensure a milestone is not claimed after it has expired.
  * **`totalDonations`:** A folding function to recalculate sums for verification.

### IO Helpers

  * **`writeValidator`:** Serializes the compiled code to a `.plutus` file.
  * **`toBech32ScriptAddress`:** Uses `Cardano.Api` to convert the validator hash into a usable Testnet or Mainnet address string (Bech32).

-----

## 6\. 🧪 Practical Usage Example

The `main` function in this module serves as a deployment script.

```haskell
-- 1. Compiles the validator
-- 2. Writes it to "campaign-finance.plutus"
-- 3. Prints the Script Hash and Testnet Address
main :: IO ()
main = do
    -- ... definition of network ...
    writeValidator "campaign-finance.plutus" validator
    
    -- Print transparency and app features log
    putStrLn "TRANSPARENCY FEATURES: ..."
```

**To Run:**
Execute `cabal run campaign-finance` in your terminal. This will generate the file needed to build transactions using the Cardano CLI or a frontend mesh.

-----

## 7\. 🧷 Testing Strategy

Due to the complexity of state transitions, testing should focus on:

  * **State Continuity:** Ensure the `cdDonors` list grows strictly monotonically (never deletes old donors).
  * **Grant Lifecycle:** Simulate a full flow: Create Grant $\to$ Release Milestone 1 $\to$ Release Milestone 2.
  * **Negative Testing:**
      * Attempt to release a milestone with the wrong `mReportHash`.
      * Attempt to spend more than the `cPerTxLimit`.
      * Attempt to modify the datum without the Treasurer's signature.

-----

## 8\. ✅ Best Practices

  * **Datum Management:** The `cdDonors` list grows indefinitely. In a production mainnet environment, you should implement a strategy to "archive" old donors or use a Merkle Tree root to prevent the datum from exceeding the transaction size limit.
  * **Off-Chain Verification:** The `mReportHash` implies an off-chain document exists. Ensure your frontend application validates the document content against this hash before submitting a transaction.
  * **Double Satisfaction:** The contract uses `getContinuingOutputs` to ensure that the output UTXO contains the strictly updated state, preventing double-spending vulnerabilities.

-----

## 9\. 📘 Glossary of Terms

| Term | Definition |
| :--- | :--- |
| **Treasurer** | The `PubKeyHash` authorized to perform administrative actions and approve spending. |
| **Milestone** | A specific goal within a grant that triggers a partial release of funds. |
| **Report Hash** | A cryptographic proof (usually SHA-256) of a document (PDF, video) proving work was done. |
| **CampaignDatum** | The "State" of the contract. It changes with every transaction to reflect new balances and history. |
| **Audit Trail** | The `[DonorRecord]` list stored on-chain, providing immutable proof of all incoming funds. |
| **Redeemer** | The `CampaignAction` data type that tells the script *which* logic path to execute (e.g., Donate vs. Disburse). |
| **Lovelace** | The smallest unit of ADA ($1 \text{ ADA} = 1,000,000 \text{ Lovelace}$). |
| **Interval** | A Plutus type representing a range of time, used here to validate deadlines. |

-----

