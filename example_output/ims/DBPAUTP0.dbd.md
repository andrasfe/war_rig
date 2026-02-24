# DBPAUTP0

**File**: `ims/DBPAUTP0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-24 03:52:49.625058

## Purpose

This file is the source code for the IMS Database Definition (DBD) named DBPAUTP0. It defines a HIDAM database using VSAM access with dataset DDPAUTP0 in dataset group 1. The structure includes root segment PAUTSUM0 (Pending Authorization Summary) with unique sequential key ACCNTID and child segment PAUTDTL1 (Pending Authorization Details), plus a logical child index PAUTINDX to dataset DBPAUTX0.

**Business Context**: Manages pending authorization summaries and details for accounts, keyed by account ID, likely supporting financial or authorization processing workflows.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTP0 | IOType.FILE_VSAM | Primary dataset for storing PAUTSUM0 root and PAUTDTL1 child segments |
| DBPAUTX0 | IOType.FILE_VSAM | Secondary index dataset referenced by logical child PAUTINDX under PAUTSUM0 |

## Business Rules

- **BR001**: PAUTSUM0 segment is the root with unique sequential key on ACCNTID field (packed decimal, bytes 1-6)
- **BR002**: PAUTDTL1 segment is a direct child of PAUTSUM0 with sequential unique key PAUT9CTS (character, bytes 1-8)

## Open Questions

- ? What specific programs or JCL jobs reference and use this DBD/DBPAUTP0?
  - Context: DBD source defines structure but does not list consumer programs; system context mentions datasets like RECON1-RECON3 and DFSURGU1 but unverified in this file
- ? What are the full field layouts beyond the keys (e.g., other fields in PAUTSUM0 BYTES=100 and PAUTDTL1 BYTES=200)?
  - Context: Only key fields explicitly defined; other bytes implied but not named
