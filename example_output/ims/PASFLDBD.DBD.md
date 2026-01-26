# PASFLDBD

**File**: `ims/PASFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 14:23:47.921690

## Purpose

This file contains the DBDGEN source defining the IMS database PASFLDBD with GSAM and BSAM access methods and no password. It specifies dataset group DSG001 using input dataset PASFILIP (DD1) and output dataset PASFILOP (DD2), both with fixed record length of 100 bytes and RECFM=F. The definition is terminated with DBDGEN, FINISH, and END statements.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILIP | IOType.FILE_SEQUENTIAL | Input dataset referenced as DD1 in dataset group DSG001 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.FILE_SEQUENTIAL | Output dataset referenced as DD2 in dataset group DSG001 |

## Business Rules

- **BR001**: IMS database PASFLDBD is defined with access methods GSAM and BSAM, and no password protection
- **BR002**: Dataset group DSG001 uses DD1=PASFILIP and DD2=PASFILOP with record format RECFM=F and length 100
