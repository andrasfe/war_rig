# DBPAUTX0

**File**: `ims/DBPAUTX0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 14:24:07.627925

## Purpose

This DBD source defines the IMS hierarchical database DBPAUTX0 with INDEX/VSAM access and protection. It specifies dataset group DSG001 using VSAM dataset DDPAUTX0 of size 4096 bytes. The database has a single root segment PAUTINDX (6 bytes) with unique packed sequence key field INDXSEQ, and a logical child relationship to PAUTSUM0 segment in database DBPAUTP0 via index ACCNTID.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTX0 | IOType.FILE_VSAM | Primary VSAM dataset assigned to DD1 for dataset group DSG001 |
| INDXSEQ | IOType.IMS_SEGMENT | Unique packed sequence key field (6 bytes) for root segment PAUTINDX, starting at position 1, used for primary index access |

## Business Rules

- **BR001**: PAUTINDX segment is defined as root segment with no parent
- **BR002**: PAUTINDX segment frequency is estimated at 100000 occurrences

## Open Questions

- ? What is the exact business purpose of DBPAUTX0 and segments PAUTINDX/PAUTSUM0?
  - Context: Names suggest Payment Authorization indexing (PAUT=Payment Auth?), but no descriptive comments in source
- ? Details of logical child PAUTSUM0 in DBPAUTP0?
  - Context: Referenced but not defined here; requires cross-reference to DBPAUTP0 DBD
