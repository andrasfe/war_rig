# PASFLDBD

**File**: `ims/PASFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-24 03:56:38.801326

## Purpose

This file is the DBDGEN source defining the IMS database PASFLDBD. It specifies GSAM and BSAM access methods with no password protection. A single dataset group DSG001 defines input dataset DD1=PASFILIP and output dataset DD2=PASFILOP with fixed record format (RECFM=F) and length 100.

**Business Context**: IMS database definition supporting sequential and random access to PAS files for input processing and output generation

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILIP | IOType.FILE_SEQUENTIAL | Input dataset (DD1) for the PASFLDBD IMS database |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.FILE_SEQUENTIAL | Output dataset (DD2) for the PASFLDBD IMS database |

## Business Rules

- **BR001**: Database supports GSAM for random access and BSAM for sequential access
- **BR002**: No password protection is required for database access
- **BR003**: Datasets use fixed record format (RECFM=F) with record length of 100 bytes

## Open Questions

- ? What is the business purpose of the PAS files (PASFILIP/PASFILOP)?
  - Context: File names suggest PAS file processing, but no details in source
- ? Are there any associated PSB or PCB definitions?
  - Context: DBD defines database but no application program control blocks referenced
