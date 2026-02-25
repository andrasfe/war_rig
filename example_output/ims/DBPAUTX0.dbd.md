# DBPAUTX0

**File**: `ims/DBPAUTX0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-25 15:26:27.988041

## Purpose

This DBD source defines the IMS database DBPAUTX0 as an index database using VSAM access with dataset DDPAUTX0. It specifies a single root segment PAUTINDX (6 bytes) with a unique packed decimal sequence field INDXSEQ at positions 1-6 for key-based access. The database provides logical child access to PAUTSUM0 segments in the physical database DBPAUTP0 via the ACCNTID total segment index.

**Business Context**: Supports efficient index-based retrieval of account summary segments (PAUTSUM0) by account ID or sequence key in an IMS hierarchical database environment.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTX0 | IOType.FILE_VSAM | VSAM dataset containing the index database physical storage. |
| PAUTINDX.INDXSEQ | IOType.IMS_SEGMENT | Unique packed decimal sequence key field (positions 1-6, TYPE=P) in root segment PAUTINDX, used as primary input for index database access. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Logical child summary segments in database DBPAUTP0, accessed via the ACCNTID index from the PAUTINDX root segment. |

## Business Rules

- **BR001**: The root segment PAUTINDX provides index-based access to PAUTSUM0 summary segments using the ACCNTID index total segment index.

## Open Questions

- ? What is the exact business meaning of INDXSEQ and ACCNTID fields?
  - Context: Field names suggest account indexing (e.g., account ID), but specific data semantics not defined in DBD.
- ? Are there additional segments or fields not shown in this simplified DBD?
  - Context: Only one segment defined; production DBDs often have more.
