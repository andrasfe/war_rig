# DBPAUTP0

**File**: `ims/DBPAUTP0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 17:38:32.016946

## Purpose

This file contains the IMS Database Definition (DBD) source code for defining database DBPAUTP0 as a HIDAM database with VSAM access method and associated user exits for key, data, and logging. It specifies one dataset group DSG001 with VSAM dataset DDPAUTP0 (4096 bytes, SCAN=3). The database structure includes a root segment PAUTSUM0 (Pending Authorization Summary, 100 bytes, key ACCNTID at position 1, 6 packed bytes, unique sequence, twin backward pointers, logical child PAUTINDX from DBPAUTX0) and a single child segment PAUTDTL1 (Pending Authorization Details, 200 bytes under PAUTSUM0, key PAUT9CTS at position 1, 8 character bytes, unique sequence).

**Business Context**: Manages hierarchical storage of pending authorization summaries by account ID with associated detail records, likely for financial transaction or payment authorization processing.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTP0 | IOType.FILE_VSAM | VSAM dataset referenced as the primary storage for the HIDAM database segments (Dataset Group 1, DD1) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTP0 | IOType.FILE_VSAM | VSAM dataset serving as output storage for the HIDAM database segments (same as input reference for database persistence) |

## Business Rules

- **BR001**: Root segment PAUTSUM0 uses unique sequential key ACCNTID (packed decimal, 6 bytes starting at offset 1)
- **BR002**: Child segment PAUTDTL1 subordinate to PAUTSUM0 root with unique sequential key PAUT9CTS (character, 8 bytes starting at offset 1)

## Open Questions

- ? Precise business semantics of fields ACCNTID and PAUT9CTS
  - Context: Names suggest account ID and pending auth counter/timestamp/sequence, but no explicit descriptions in DBD
- ? Details of logical child PAUTINDX from DBPAUTX0
  - Context: References external database DBPAUTX0 for index pointer, but no further definition here
