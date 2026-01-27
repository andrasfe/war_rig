# DBPAUTX0

**File**: `ims/DBPAUTX0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 02:41:28.790384

## Purpose

This is a Database Definition (DBD) source file for the IMS hierarchical database named DBPAUTX0. It defines an indexed VSAM-protected database with a single dataset group DSG001 mapped to DDPAUTX0 and one root segment PAUTINDX of 6 bytes containing a unique packed sequence key field INDXSEQ. The PAUTINDX segment establishes a logical child (LCHILD) relationship to the PAUTSUM0 segment in the DBPAUTP0 database via the ACCNTID index.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTX0 | IOType.FILE_VSAM | Physical dataset for storing the DBPAUTX0 IMS database segments |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTX0 | IOType.FILE_VSAM | Physical dataset for storing the DBPAUTX0 IMS database segments (read/write access implied by VSAM INDEX access method) |

## Business Rules

- **BR001**: The INDXSEQ field serves as a unique sequence key for the PAUTINDX root segment

## Open Questions

- ? What is the business purpose and content of the PAUTINDX segment?
  - Context: No descriptive comments or field-level documentation beyond key definition
- ? Details of the ACCNTID index and PAUTSUM0 segment structure
  - Context: LCHILD references external segment in DBPAUTP0; not defined in this DBD
- ? How is this DBD used in application programs?
  - Context: DBD is a definition; no calling programs identifiable from this file
