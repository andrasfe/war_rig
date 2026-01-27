# PASFLDBD

**File**: `ims/PASFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 02:42:44.390903

## Purpose

This DBD source file defines the IMS database PASFLDBD using DBDGEN. It specifies access methods GSAM and BSAM with no password protection. The database uses dataset group DSG001 with input dataset DD1=PASFILIP, output dataset DD2=PASFILOP, fixed record length of 100 bytes, and RECFM=F.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILIP | IOType.FILE_SEQUENTIAL | Input dataset (DD1) for IMS database PASFLDBD dataset group DSG001 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.FILE_SEQUENTIAL | Output dataset (DD2) for IMS database PASFLDBD dataset group DSG001 |

## Business Rules

- **BR001**: Database access methods are GSAM (generalized sequential access method) and BSAM (basic sequential access method)
- **BR002**: No password protection required for the database
- **BR003**: Dataset records are fixed length of 100 bytes with RECFM=F
