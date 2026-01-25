# DBPAUTP0

**File**: `ims/DBPAUTP0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-25 18:32:34.255043

## Purpose

IMS Database Definition (DBD) for HIDAM database DBPAUTP0 using VSAM access. Defines root segment PAUTSUM0 (Pending Authorization Summary) with key field ACCNTID and child segment PAUTDTL1 (Pending Authorization Details) with key field PAUT9CTS. Includes dataset group DSG001 with VSAM dataset DDPAUTP0 and logical child PAUTINDX to database DBPAUTX0.

**Business Context**: Pending authorization summary and details for accounts

## Business Rules

- **BR001**: Root segment PAUTSUM0 is accessed via sequential unique key ACCNTID starting at position 1
- **BR002**: Child segment PAUTDTL1 is parented by PAUTSUM0 and accessed via sequential unique key PAUT9CTS starting at position 1

## Open Questions

- ? Purpose and structure of referenced database DBPAUTX0
  - Context: LCHILD NAME=(PAUTINDX,DBPAUTX0) references it but no definition provided in this DBD
- ? Details of EXIT routine and LOG processing
  - Context: EXIT=(*,KEY,DATA,NOPATH,(NOCASCADE),LOG) specified but no further details in source
