# PADFLDBD

**File**: `ims/PADFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 02:42:35.201465

## Purpose

This DBD source file defines the IMS database PADFLDBD using GSAM as the primary access method and BSAM as secondary. It specifies a single dataset group DSG001 with input dataset DD1=PADFILIP and output dataset DD2=PADFILOP, both using fixed-length records (RECFM=F, length 200). The file includes assembly information, IMS version 15.1, and Apache License 2.0 copyright notice.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PADFILIP | IOType.FILE_SEQUENTIAL | Input dataset referenced in dataset group DSG001 for the PADFLDBD database |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PADFILOP | IOType.FILE_SEQUENTIAL | Output dataset referenced in dataset group DSG001 for the PADFLDBD database |
