# DBPAUTP0

**File**: `ims/DBPAUTP0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-25 15:22:14.369249

## Purpose

This is a Database Definition (DBD) source file for the IMS database DBPAUTP0, specifying a HIDAM database with VSAM access and no password. It defines one dataset group DSG001 using logical dataset DDPAUTP0 of size 4096 with scan value 3. The database has two segments in a hierarchical structure: root segment PAUTSUM0 (Pending Authorization Summary) with key field ACCNTID and child segment PAUTDTL1 (Pending Authorization Details) with key field PAUT9CTS.

**Business Context**: Defines structure for storing and accessing pending authorization summary and detail data for accounts (e.g., ACCNTID), likely supporting financial transaction processing in a card demo environment.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTP0 | IOType.FILE_VSAM | Primary dataset referenced in dataset group DSG001 for storing segments of DBPAUTP0 database |
| DBPAUTX0 | IOType.OTHER | Index database referenced via LCHILD pointer from PAUTSUM0 segment under PAUTINDX |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.FILE_VSAM | IMS database defined by this DBD source, targeted for data storage as per system context |

## Business Rules

- **BR001**: PAUTSUM0 is root segment (PARENT=0) with sequential unique packed key ACCNTID starting at offset 1
- **BR002**: PAUTDTL1 is child segment of PAUTSUM0 with sequential unique character key PAUT9CTS starting at offset 1

## Open Questions

- ? Specific programs or JCL that assemble and use this DBD
  - Context: Source defines the database but does not reference calling programs; system context lists related datasets and libraries
- ? Detailed field layouts beyond keys (e.g., other fields in segments)
  - Context: Only key fields explicitly defined; other bytes implied but not named
