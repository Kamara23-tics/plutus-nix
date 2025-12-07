-----

# 🧾 Detailed Tutorial: Understanding and Using `Identity.hs`

This tutorial covers the multi-faceted `Identity.hs` module. This contract establishes a comprehensive on-chain framework for **Verifiable Credentials (VCs)**, **Soul-Bound KYC Badges**, **Decentralized Identifiers (DIDs)**, and **Zero-Knowledge Proof Gates**. The core purpose is to provide decentralized identity management and granular, privacy-preserving access control.

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
    Provides the necessary types and utilities for V2 validation, including `PubKeyHash`, `POSIXTime`, and `txSignedBy` for authorization checks.

  * **Plutus.V1.Ledger.Interval:**
    Used extensively for checking **expiration dates** (`vcExpiry`, `kycExpiry`) against the transaction time range (`txRange`).

### Utility, Serialization, and Builtins

  * **PlutusTx:**
    Enables script compilation and data serialization for all five custom data types (`VCRef`, `KYCBadge`, etc.).

  * **Builtins:**
    Used for `BuiltinByteString`, which stores non-serializable data like cryptographic hashes (e.g., `vcDataHash`, `zkProofHash`) and DID document references.

  * **Cardano.Api:**
    Integrated for off-chain utility, specifically to generate the **Bech32** address for easy deployment via the Cardano CLI.

-----

## 2\. 🗃️ Data Structures

This contract uses a **Sum Type** (`IdentityDatum`) to allow one script address to manage five distinct identity-related components.

### `IdentityDatum` (The State)

The primary datum dictates the context of the UTXO being spent:

  * **`CredentialDatum VCRef`**: Holds a reference to an off-chain Verifiable Credential.
  * **`KYCBadgeDatum KYCBadge`**: Holds a non-transferable identity badge status.
  * **`DIDDatum DIDDoc`**: Holds a reference to a Decentralized Identifier Document.
  * **`RevocationDatum [RevocationEntry]`**: Holds the list of permanently revoked credentials.
  * **`ZKGateDatum ZKGate`**: Holds the configuration for a Zero-Knowledge Proof access gate.

### Key Nested Structures

  * **`KYCBadge`**: Tracks `kycLevel` (1-5), `kycHolder` (ensures soul-bound nature), and `kycRevoked` status.
  * **`VCRef`**: Stores `vcSchemaHash` and `vcDataHash`, crucial for off-chain data integrity.
  * **`ZKGate`**: Defines access rules: `zkRequiredCred`, `zkMinLevel`, and `zkAllowedIssuers` (whitelist).

### `IdentityAction` (The Redeemer)

The actions define how the state changes or is verified:

  * **Issuance/Update:** `IssueCredential`, `IssueKYCBadge`, `UpdateDID`, `AddRevocation`.
  * **Revocation/Burn:** `RevokeCredential`, `RevokeKYCBadge`, `BurnKYCBadge`.
  * **Verification:** `VerifyWithZK`.

-----

## 3\. 🧠 Core Validator Logic

### `mkValidator`

The validator contains eight distinct branches, each enforcing specific security and business logic rules based on the `IdentityAction`.

| Action | Primary Authorization Check | Key Constraint Checks | State Change |
| :--- | :--- | :--- | :--- |
| **IssueCredential** | `vcIssuer` signature | `validExpiry`, `notRevoked` | **Locked** (Datum updated) |
| **RevokeCredential** | `vcIssuer` signature | `notRevoked` | Marks `vcRevoked = True` |
| **IssueKYCBadge** | `kycIssuer` signature | `validKYCLevel` (1-5) | **Locked** (Soul-bound) |
| **RevokeKYCBadge** | `kycIssuer` signature | `badgeNotRevoked` | Marks `kycRevoked = True` |
| **UpdateDID** | `didController` signature | `didDocUpdated` (hash changed) | `timestampUpdated` |
| **VerifyWithZK** | `zkVerifier` signature | `proofHashValid` (match) | **Consumed** (One-time access) |
| **BurnKYCBadge** | `kycHolder` signature | `badgeBurned` (No output) | **Consumed** (Deleted) |
| **AddRevocation** | `revIssuer` signature | `entryAdded` (List length + 1) | **Updated** (List continues) |

**Key Security Checks:**

  * **Soul-Binding (`badgeLocked`):** For `IssueKYCBadge`, the validator ensures the UTXO containing the badge token must continue to the same script address, thus preventing direct transfer by the holder.
  * **Time Control (`validExpiry`):** Ensures no expired credentials are issued.
  * **State Continuity:** For updates (like `RevokeCredential` or `UpdateDID`), the validator explicitly checks the continuing output (`contOutputs`) to ensure the state change (e.g., `vcRevoked = True`, new `didDocHash`) was correctly recorded in the new datum.

-----

## 4\. ⚙️ Validator Script Compilation

### `mkValidatorUntyped`

This is the standard wrapper that facilitates communication between the Haskell type system and the Plutus Core environment. It uses `unsafeFromBuiltinData` to cast the raw on-chain data (`BuiltinData`) into the expected types (`IdentityDatum`, `IdentityAction`, `ScriptContext`).

### `validator`

Compiles the wrapped logic into the Plutus Core script using `PlutusTx.compile`, making it ready for serialization and on-chain deployment.

-----

## 5\. 🔧 Helper Functions

### Business Logic Helpers

  * **`isExpired` / `isNotExpired`**: Utility functions that check credential timestamps against the transaction time range (`txRange`).
  * **`isTrustedIssuer` / `isRevoked`**: Placeholder checks that would interface with the revocation list and trusted issuer whitelists (though some are simplified to `True` for this example).
  * **`meetsKYCLevel`**: Simple comparison logic for the Zero-Knowledge Gate requirements.

### IO Helpers

  * **`plutusValidatorHash`**: Computes the script hash from the compiled `Validator`.
  * **`toBech32ScriptAddress`**: Converts the script hash into a standardized Bech32 address, enabling direct use with the Cardano CLI.
  * **`writeValidator`**: Writes the serialized script to a `.plutus` file.

-----

## 6\. 🧪 Practical Usage Example

The `main` function executes the build process, generating the deployable file and essential addresses:

```haskell
-- 1. Compiles the validator
-- 2. Writes it to "identity.plutus"
writeValidator "identity.plutus" validator

-- 3. Prints key deployment information
putStrLn "==================================================="
putStrLn "     IDENTITY & VERIFIABLE CREDENTIALS SYSTEM"
putStrLn $ "Bech32 Address: " <> bech32
```

**Typical Flow:**
An issuer initiates a transaction using the **`IssueCredential`** redeemer. This locks the `VCRef` datum at the validator address. A verifier later queries this UTXO (ideally using a **Reference Input**) to check the issuance details and expiration without needing to spend it.

-----

## 7\. 🧷 Testing Strategy

  * **Revocation Flow:** Test the lifecycle: Issue -\> Revoke -\> Attempt to Spend (should fail due to revocation flag).
  * **Time Boundaries:** Test credentials being spent immediately *before* the `vcExpiry` (success) and immediately *after* (failure).
  * **KYC Burn:** Verify that only the `kycHolder` can initiate `BurnKYCBadge` and that the UTXO is consumed (`null contOutputs`).
  * **ZK Gate Integrity:** Ensure a transaction using `VerifyWithZK` fails if the `proofHash` in the redeemer does not match `zkProofHash` in the locked datum.
  * **DID Update:** Verify that `UpdateDID` requires both a signature from the controller and a change in the `didDocHash` field to prevent trivial updates.

-----

## 8\. ✅ Best Practices

  * **Privacy-Preserving Design:** The design uses cryptographic hashes (`vcDataHash`, `zkProofHash`) to keep sensitive data off-chain (e.g., on IPFS), while the Plutus contract enforces the rules based only on the hash.
  * **State Machine Management:** This contract utilizes the UTXO model to implement complex state machines (Issuance, Revocation, Update). Each action (redeemer) verifies the input state and ensures the correct output state (new datum) is created or the UTXO is consumed.
  * **Reference Inputs:** For read-only actions like verifying credentials or checking a revocation entry, a real application would utilize Plutus's **Reference Inputs** feature (introduced in the Babbage era) to check the datum without consuming the UTXO, preserving the state indefinitely.

-----

## 9\. 📘 Glossary of Terms

| Term | Definition |
| :--- | :--- |
| **VCRef** | Verifiable Credential Reference. On-chain metadata about a credential, typically containing hashes of the off-chain data. |
| **SBT** | Soul-Bound Token. A non-transferable token (like the `KYCBadge`) linked permanently to the holder's key. |
| **DID** | Decentralized Identifier. A globally unique identifier managed by the `didController`. |
| **ZK Gate** | Zero-Knowledge Gate. A mechanism that grants access upon presentation of a valid cryptographic proof, without revealing the underlying data. |
| **BuiltinByteString** | A Plutus-specific data type used to store raw binary data, such as cryptographic hashes. |
| **Revocation List** | An on-chain, append-only list that marks credentials as permanently invalid. |
| **Controller** | The party authorized to update a DID document. |
| **POSIXTime** | A time representation used in Plutus for defining deadlines and issuance timestamps. |