# PASFLDBD

**File**: `ims/PASFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 02:33:26.709941

## Purpose

This DBDGEN source file defines the IMS database PASFLDBD with GSAM and BSAM access methods and no password protection. It specifies dataset group DSG001 using DD1=PASFILIP (input) and DD2=PASFILOP (output) datasets, both with fixed-length records of 100 bytes and RECFM=F. The definition was generated on 04/21/2023 for IMS version 15.1.

**Business Context**: IMS database definition supporting sequential file access for an application handling PASFL data (likely passenger flight files based on naming)

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILIP | IOType.FILE_SEQUENTIAL | Input dataset DD1 for PASFLDBD database dataset group DSG001 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.FILE_SEQUENTIAL | Output dataset DD2 for PASFLDBD database dataset group DSG001 |

## Business Rules

- **BR001**: Database supports GSAM and BSAM access methods
- **BR002**: No password protection required for database access
- **BR003**: Datasets use fixed-length records of 100 bytes with RECFM=F
