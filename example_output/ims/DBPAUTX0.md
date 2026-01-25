# DBPAUTX0

**File**: `ims/DBPAUTX0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-25 18:37:47.523389

## Purpose

Defines IMS database DBPAUTX0 as an indexed VSAM database. Specifies dataset DDPAUTX0 and root segment PAUTINDX with unique sequence key INDXSEQ. Includes logical child pointer to PAUTSUM0 segment in DBPAUTP0 via ACCNTID index.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTINDX | IOType.IMS_SEGMENT | Root segment (PARENT=0, BYTES=6) accessible via DL/I calls, containing key field INDXSEQ |
| PAUTSUM0 | IOType.IMS_SEGMENT | Logical child segment from database DBPAUTP0, accessed via index ACCNTID |
| DDPAUTX0 | IOType.FILE_VSAM | VSAM dataset (DSG001) holding PAUTINDX segments, accessed via DL/I in application programs; no runtime I/O in DBD itself |

## Business Rules

- **BR001**: Enforces unique sequence ordering on key field INDXSEQ

## Open Questions

- ? Precise business purpose of DBPAUTX0 (e.g., what PAUTINDX/PAUTSUM0 represent)
  - Context: DBD provides structural definition only, no business description
- ? Update behavior for this index database
  - Context: DBD defines structure but not read/write rules
