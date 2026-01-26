# PADFLDBD

**File**: `ims/PADFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 15:14:19.130784

## Purpose

This file is the source code for the IMS Database Definition (DBD) named PADFLDBD, generated via DBDGEN. It specifies GSAM and BSAM access methods for a simple database with no segments. A single dataset group DSG001 is defined, including input dataset PADFILIP (DD1) and output dataset PADFILOP (DD2), both using fixed-length records of 200 bytes (RECFM=F).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PADFILIP | IOType.FILE_SEQUENTIAL | Input dataset defined as DD1 in dataset group DSG001 for the PADFLDBD database |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PADFILOP | IOType.FILE_SEQUENTIAL | Output dataset defined as DD2 in dataset group DSG001 for the PADFLDBD database |

## Business Rules

- **BR001**: Access to the PADFLDBD IMS database is restricted to GSAM and BSAM methods only
- **BR002**: Datasets in DSG001 must use fixed-length records (RECFM=F) of exactly 200 bytes

## Open Questions

- ? No IMS segments or fields are defined in this DBD
  - Context: The DBD defines only datasets and access methods without any SEGMENT, FIELD, or DATASET macros beyond DSG001
