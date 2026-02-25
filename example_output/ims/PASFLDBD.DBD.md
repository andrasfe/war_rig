# PASFLDBD

**File**: `ims/PASFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-25 15:22:29.803944

## Purpose

This DBDGEN source file defines the IMS database PASFLDBD with GSAM and BSAM access methods and no password protection. It specifies a single dataset group DSG001 containing two fixed-block datasets (RECFM=F, LRECL=100): PASFILIP as input (DD1) and PASFILOP as output (DD2). The definition was assembled with IMS version 15.1 on 04/21/2023.

**Business Context**: IMS database definition for PASFL-related input/output file processing

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILIP | IOType.FILE_SEQUENTIAL | Input dataset (DD1) in dataset group DSG001 for the PASFLDBD GSAM database, with fixed-length records of 100 bytes |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.FILE_SEQUENTIAL | Output dataset (DD2) in dataset group DSG001 for the PASFLDBD GSAM database, with fixed-length records of 100 bytes |

## Open Questions

- ? What does 'PASFL' acronym represent?
  - Context: Not specified in the DBD source; likely application-specific (e.g., Passenger Flight Load?)
- ? Are there associated segment or field definitions?
  - Context: No segments or PCB defined in this GSAM DBD; only dataset group specified
