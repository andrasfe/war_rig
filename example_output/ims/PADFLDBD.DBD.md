# PADFLDBD

**File**: `ims/PADFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-24 03:56:17.742955

## Purpose

This is an IMS DBDGEN source file defining the PADFLDBD database with access methods GSAM and BSAM. It specifies dataset group DSG001 with input dataset DD1=PADFILIP and output dataset DD2=PADFILOP, both using fixed-length records of 200 bytes (RECFM=F). The definition was generated for IMS version 15.1 on 04/21/2023.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PADFILIP | IOType.FILE_SEQUENTIAL | Input dataset (DD1) for the PADFLDBD IMS database, accessed via GSAM |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PADFILOP | IOType.FILE_SEQUENTIAL | Output dataset (DD2) for the PADFLDBD IMS database, accessed via BSAM |

## Business Rules

- **BR001**: Database access methods are GSAM (for input) and BSAM (for output)
- **BR002**: Datasets use fixed record format (RECFM=F) with record length of 200 bytes

## Open Questions

- ? What is the specific business purpose of the PADFLDBD database?
  - Context: File name suggests padding file input/output, but no functional description provided in source
