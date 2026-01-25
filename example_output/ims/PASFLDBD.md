# PASFLDBD

**File**: `ims/PASFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-25 18:35:30.323357

## Purpose

This DBD source defines the IMS database PASFLDBD using GSAM and BSAM access methods with no password protection. It specifies dataset group DSG001 including input dataset DD1=PASFILIP and output dataset DD2=PASFILOP. Records are fixed length 100 bytes with RECFM=F.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILIP | IOType.FILE_SEQUENTIAL | Input dataset referenced as DD1 in dataset group DSG001 for PASFLDBD database |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.FILE_SEQUENTIAL | Output dataset referenced as DD2 in dataset group DSG001 for PASFLDBD database |

## Business Rules

- **BR001**: Dataset records for PASFLDBD have fixed length of 100 bytes with RECFM=F

## Open Questions

- ? What is the specific business purpose of the PASFLDBD database?
  - Context: DBD source defines physical structure but not application usage
