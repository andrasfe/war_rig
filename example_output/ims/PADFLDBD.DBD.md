# PADFLDBD

**File**: `ims/PADFLDBD.DBD`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 17:40:54.146429

## Purpose

This file is a DBD (Database Definition) source for the IMS database PADFLDBD, defining it with GSAM and BSAM access methods and no password. It specifies a single dataset group DSG001 with input dataset PADFILIP (DD1) and output dataset PADFILOP (DD2), both using fixed-length records (RECFM=F, length 200). The DBDGEN assembly details include IMS version 15.1 and generation date 04/21/2023.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PADFILIP | IOType.FILE_SEQUENTIAL | Input dataset (DD1) in dataset group DSG001 for the PADFLDBD IMS database, fixed record format length 200 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PADFILOP | IOType.FILE_SEQUENTIAL | Output dataset (DD2) in dataset group DSG001 for the PADFLDBD IMS database, fixed record format length 200 |
