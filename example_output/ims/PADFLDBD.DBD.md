# PADFLDBD

**File**: `ims/PADFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 14:21:22.137536

## Purpose

This file is a DBDGEN source/listing defining the IMS database PADFLDBD with access methods GSAM and BSAM. It specifies dataset group DSG001 including input dataset DD1=PADFILIP and output dataset DD2=PADFILOP, both with fixed-length records of 200 bytes and RECFM=F. The definition was generated on 04/21/2023 for IMS version 15.1.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PADFILIP | IOType.FILE_SEQUENTIAL | Input dataset (DD1) in dataset group DSG001 for the PADFLDBD database, with fixed 200-byte records |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PADFILOP | IOType.FILE_SEQUENTIAL | Output dataset (DD2) in dataset group DSG001 for the PADFLDBD database, with fixed 200-byte records |

## Open Questions

- ? What is the specific business purpose of the PADFLDBD database?
  - Context: No descriptive comments or context provided beyond dataset names and technical specs
