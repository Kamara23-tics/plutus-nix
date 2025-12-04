Below is a fully structured tutorial for your Identity, Verifiable Credentials & KYC Smart Contract, written in the exact same style, tone, formatting, and structure as the Vesting.hs tutorial you provided — including:

✅ Table of Contents
✅ Section headings
✅ Clean explanations
✅ Glossary
❗ No code was modified, only documented.
❗ Exactly mirrors your style request.

🧾 Detailed Tutorial: Understanding and Using IdentityValidator.hs

This tutorial explains the Identity, Verifiable Credential, KYC, DID, Revocation & ZK Gate on-chain identity system built in your smart contract.
It covers the system’s purpose, core data structures, validator logic, signature checks, expiration validation, ZK-proof gating, DID updates, and KYC badge rules.

This module provides a complete decentralized identity framework using Plutus V2.

📚 Table of Contents

📦 Imports Overview

🗃 Data Structures

⚙ Helper Validation Functions

🧠 Core Validator Logic

🧰 Untyped Wrapper & Validator Script

🏠 Script Address + Bech32 Conversion

💾 File Output

🧪 Practical Usage Example

🧷 Testing Strategy

✅ Best Practices

📘 Glossary of Terms

1. 📦 Imports Overview
🔹 Plutus API Modules

Plutus.V2.Ledger.Api
Provides ScriptContext, TxInfo, POSIXTime, PubKeyHash, datums, redeemers, and validator representation.

Plutus.V2.Ledger.Contexts
Used for signature checks and reading continuing outputs.

Plutus.V1.Ledger.Interval
Provides time-interval validation such as contains, from, and to.

🔹 Utility Modules

PlutusTx / PlutusTx.Prelude
Enables on-chain compilation, serialization, and Plutus-specific logic.

Codec.Serialise
Serializes the validator to .plutus file format.

Cardano.Api
Creates Bech32 script addresses.

2. 🗃 Data Structures

Your contract defines a multi-model identity system, each represented as datum variants.

🔹 VCRef — Verifiable Credential Reference

Contains:

Issuer & Subject PKH

Schema hash

Data hash

Expiry & issuance timestamps

Revocation status

Credential type (e.g., "KYC", "DIPLOMA")

🔹 KYCBadge — Soul-Bound KYC Token

Bound to holder (kycHolder)

KYC level (1–5)

Jurisdiction

Issuer

Expiry, issue date

Revocation flag

Metadata reference (e.g., IPFS hash)

🔹 DIDDoc — DID Document

Stores:

Controller

DID document hash

Public keys

Service endpoints

Created & updated timestamps

🔹 RevocationEntry

For revocation lists:

Credential hash

Timestamp

Reason

Issuer who revoked

🔹 ZKGate

Defines:

Required credential type

Minimum KYC level

ZK proof hash

Authorized verifier

Allowed issuers list

🔹 IdentityDatum (Sum-type Datum)

A unified datum for all identity operations:

CredentialDatum VCRef
KYCBadgeDatum KYCBadge
DIDDatum DIDDoc
RevocationDatum [RevocationEntry]
ZKGateDatum ZKGate

🔹 IdentityAction (Redeemer Actions)

Includes actions such as:

IssueCredential

RevokeCredential reason timestamp

IssueKYCBadge

RevokeKYCBadge reason

UpdateDID

VerifyWithZK proofHash

BurnKYCBadge

AddRevocation entry

3. ⚙ Helper Validation Functions

The contract uses several reusable logic helpers:

isExpired & isNotExpired

Verifies expiry using POSIXTime ranges.

isTrustedIssuer

Checks if issuer is in a trusted issuer list.

isRevoked

Checks if a credential hash appears in a revocation list.

meetsKYCLevel

Checks KYC >= required level.

These helpers reduce contract bloat and improve clarity.

4. 🧠 Core Validator Logic

The core validator:

mkValidator :: IdentityDatum -> IdentityAction -> ScriptContext -> Bool


Each datum type only allows specific actions.

🔹 CredentialDatum + IssueCredential

Checks:

Issuer signature

Not revoked

Expiry is valid

Output UTxO must continue

🔹 CredentialDatum + RevokeCredential

Checks:

Issuer signature

Credential not already revoked

New output must mark it revoked

Revocation list must be updated

🔹 KYCBadgeDatum + IssueKYCBadge

Checks:

Issuer signature

Level 1–5

Not revoked

Soul-bound: exactly one continuing output

🔹 KYCBadgeDatum + RevokeKYCBadge

Checks:

Issuer signature

Badge not already revoked

Continuing output must update revoked flag

🔹 KYCBadgeDatum + BurnKYCBadge

Checks:

Holder signature

Consuming (no continuing outputs)

🔹 DIDDatum + UpdateDID

Checks:

Controller signature

DID hash changed

Updated timestamp is greater

🔹 ZKGateDatum + VerifyWithZK

Checks:

Proof hash matches

Verifier signature present

Must consume the gate (no continuing outputs)

🔹 RevocationDatum + AddRevocation

Checks:

Entry issuer signature

Exactly one output

List length increased by 1

5. 🧰 Untyped Wrapper & Validator Script

Plutus requires a low-level BuiltinData version.

mkValidatorUntyped converts types and executes mkValidator.

validator compiles it into a Plutus V2 script via Template Haskell.

6. 🏠 Script Address & Bech32 Conversion

Your contract includes:

Script hash extraction

PlutusScriptV2 serialisation

Make Shelley-Address function

Bech32 encoded address output

This generates Cardano addresses like:

addr_test1...

7. 💾 File Output

writeValidator writes:

identity.plutus


which can be used in cardano-cli and off-chain apps.

8. 🧪 Practical Usage Example

You can:

-- Save script
writeValidator "identity.plutus" validator

-- Print Bech32 testnet address
putStrLn $ toBech32ScriptAddress Testnet validator


This allows Light Wallets, DApps, or CLI tools to deploy and interact with your identity system.

9. 🧷 Testing Strategy

Recommended tests:

VC issuance → revocation flow

KYC issuance → revocation → burn

DID update: verify hash & timestamp change

ZK gate proof success/failure

Revocation list length validation

Expiration edge cases (boundary timestamps)

Incorrect signature attempts

10. ✅ Best Practices

Always enforce signature validation

Always ensure revocation is explicit and traceable

Use deterministic outputs for soul-bound tokens

ZK verification should be “consume-on-use”

Rely heavily on trace messages for debugging

Use reference inputs for pure verification operations

11. 📘 Glossary of Terms
Term	Definition
Verifiable Credential (VC)	Signed identity or authorization document.
Soul-Bound Token (SBT)	Non-transferable token bound to a wallet.
DID	Decentralized Identifier document.
ZK Proof	Proof verifying identity/attribute without revealing details.
Revocation List	On-chain list marking credentials as revoked.
Trusted Issuer	Authority allowed to issue identity items.
Continuing Output	UTxO that must survive after script execution.
Burning	Consuming an SBT so it no longer exists.
Credential Schema	Defines expected structure of off-chain data.
Proof Hash	Hash of off-chain ZK proof to verify integrity.