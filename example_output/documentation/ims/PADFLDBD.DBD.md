# PADFLDBD

**File**: `ims/PADFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-30 19:32:56.466873

## Purpose

This DBD source file defines the IMS database PADFLDBD as a GSAM database with BSAM access method and no password protection. It specifies Dataset Group 1 (DSG001) with input dataset DD1=PADFILIP and output dataset DD2=PADFILOP, both using fixed-length records of 200 bytes (RECFM=F). The definition was prepared for DBDGEN processing on IMS version 15.1.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PADFILIP | IOType.FILE_SEQUENTIAL | Input dataset (DD1) associated with Dataset Group 1 of the PADFLDBD GSAM database, fixed 200-byte records |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PADFILOP | IOType.FILE_SEQUENTIAL | Output dataset (DD2) associated with Dataset Group 1 of the PADFLDBD GSAM database, fixed 200-byte records |

## Open Questions

- ? Specific business purpose of the PADFLDBD database
  - Context: DBD source defines structure but not application-level usage
