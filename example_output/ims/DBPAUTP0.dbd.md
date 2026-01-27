# DBPAUTP0

**File**: `ims/DBPAUTP0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 02:41:14.287501

## Purpose

This is an IMS Database Definition (DBD) source file for the HIDAM database DBPAUTP0 using VSAM access. It defines one dataset group DSG001 with VSAM dataset DDPAUTP0 and two segments: root segment PAUTSUM0 (Pending Authorization Summary) with packed key field ACCNTID (6 bytes) and logical child pointer to PAUTINDX in DBPAUTX0, and child segment PAUTDTL1 (Pending Authorization Details, 200 bytes) under PAUTSUM0 with character key PAUT9CTS (8 bytes). The file ends with DBDGEN to generate the database macro for IMS application linkage.

**Business Context**: Supports pending authorization processing, likely summarizing account-level authorizations (PAUTSUM0) with associated detail records (PAUTDTL1) in a financial or transaction system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTP0 | IOType.FILE_VSAM | VSAM dataset referenced in dataset group DSG001 for database storage, size 4096 with SCAN=3 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.OTHER | Generated IMS HIDAM database definition for use by application programs |

## Business Rules

- **BR001**: PAUTSUM0 is the root segment with unique sequential packed key ACCNTID starting at position 1
- **BR002**: PAUTDTL1 is a child segment under PAUTSUM0 with unique sequential character key PAUT9CTS
