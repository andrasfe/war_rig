# PASFLDBD

**File**: `ims/PASFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-30 19:34:04.190920

## Purpose

This DBDGEN source file defines the IMS database named PASFLDBD using GSAM and BSAM access methods without password protection. It specifies a single dataset group DSG001 with input dataset DD1=PASFILIP and output dataset DD2=PASFILOP, both using fixed-length records (RECFM=F) of 100 bytes. The definition was generated on 04/21/2023 for IMS version 15.1.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILIP | IOType.FILE_SEQUENTIAL | Input dataset (DD1) for the PASFLDBD GSAM database, referenced in dataset group DSG001 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.FILE_SEQUENTIAL | Output dataset (DD2) for the PASFLDBD GSAM database, referenced in dataset group DSG001 |

## Open Questions

- ? What is the business purpose of the PASFLDBD database?
  - Context: The source provides only technical definition details (access methods, datasets) with no descriptive comments on usage or application context.
- ? Are there any associated segments or fields defined for PASFLDBD?
  - Context: No SEGMENT or FIELD macros present; only DATASET group defined, typical for simple GSAM database.
