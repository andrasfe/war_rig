# PADFLDBD

**File**: `ims/PADFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-25 15:26:04.240875

## Purpose

This DBDGEN source file defines the IMS database PADFLDBD with dual access methods GSAM and BSAM, no password protection. It specifies Dataset Group 1 (DSG001) consisting of input dataset DD1=PADFILIP and output dataset DD2=PADFILOP, with fixed-length records of 200 bytes (RECFM=F). The file is assembled into a DBD library for IMS database access.

**Business Context**: IMS database definition supporting sequential file input/output operations, likely for padding or file processing workflows

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PADFILIP | IOType.FILE_SEQUENTIAL | Input dataset defined as DD1 in Dataset Group 1 for IMS database access |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PADFILOP | IOType.FILE_SEQUENTIAL | Output dataset defined as DD2 in Dataset Group 1 for IMS database access |

## Business Rules

- **BR001**: Database supports GSAM and BSAM access methods
- **BR002**: No password protection is required for database access
- **BR003**: Database records are fixed-length (RECFM=F) with length 200 bytes
