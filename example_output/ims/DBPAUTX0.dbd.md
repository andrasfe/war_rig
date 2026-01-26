# DBPAUTX0

**File**: `ims/DBPAUTX0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 17:38:53.460678

## Purpose

This file is the source code for the IMS Database Definition (DBD) named DBPAUTX0, defining an indexed VSAM-protected IMS database. It specifies one dataset group DSG001 with VSAM dataset DDPAUTX0 of size 4096. The database has a single root segment PAUTINDX of 6 bytes with a packed sequential index field INDXSEQ starting at position 1, and a logical child segment PAUTSUM0 from database DBPAUTP0 accessed via the ACCNTID index.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTX0 | IOType.FILE_VSAM | VSAM dataset defined for dataset group DSG001 in the IMS database |

## Open Questions

- ? What is the specific business purpose of the DBPAUTX0 database and its segments PAUTINDX and PAUTSUM0?
  - Context: DBD source defines structure only; no business logic or usage described.
- ? Are there additional segments or dataset groups in the full database definition?
  - Context: Source shows only one segment and one dataset group up to DBDGEN.
