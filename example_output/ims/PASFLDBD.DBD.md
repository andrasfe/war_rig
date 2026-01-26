# PASFLDBD

**File**: `ims/PASFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 15:16:13.031113

## Purpose

This file is the source for the IMS Database Definition (DBD) named PASFLDBD. It defines a GSAM database with BSAM access using a single dataset group DSG001. The dataset group specifies DD1=PASFILIP for input and DD2=PASFILOP for output, both with fixed-length records of 100 bytes and RECFM=F.

**Business Context**: Provides IMS database definition for sequential file processing, likely supporting PASFLIP-related batch operations with dedicated input dataset PASFILIP and output dataset PASFILOP.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILIP | IOType.FILE_SEQUENTIAL | Input dataset DDNAME for the PASFLDBD GSAM database |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.FILE_SEQUENTIAL | Output dataset DDNAME for the PASFLDBD GSAM database |

## Open Questions

- ? No segment definitions present; is this a complete GSAM DBD?
  - Context: GSAM DBDs typically lack SEGMENT statements and focus on dataset groups, but confirmation from IMS documentation would verify.
- ? What application programs reference this DBD?
  - Context: DBD defines database structure but does not list using programs.
