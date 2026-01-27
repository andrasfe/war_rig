# PSBPAUTB

**File**: `ims/PSBPAUTB.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 02:42:53.716557

## Purpose

This file defines an IMS Program Specification Block (PSB) named PSBPAUTB for COBOL application programs. It specifies a database PCB named PAUTBPCB accessing DBDNAME=DBPAUTP0 with PROCOPT=AP and KEYLEN=14, sensitive to root segment PAUTSUM0 and child segment PAUTDTL1. The PSB is generated with CMPAT=YES.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.OTHER | IMS Database PCB providing access to DBDNAME=DBPAUTP0 database with PROCOPT=AP and KEYLEN=14 |
| PAUTSUM0 | IOType.IMS_SEGMENT | Root sensitive segment (PARENT=0) accessible via PAUTBPCB for application programs to retrieve/process |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child sensitive segment (PARENT=PAUTSUM0) accessible via PAUTBPCB for application programs to retrieve/process |

## Business Rules

- **BR001**: Segment hierarchy: PAUTSUM0 defined as root segment with PARENT=0; PAUTDTL1 defined as child segment with PARENT=PAUTSUM0
- **BR002**: Access permissions for PAUTBPCB set to PROCOPT=AP with KEYLEN=14

## Open Questions

- ? Precise IMS DL/I operations permitted by PROCOPT=AP
  - Context: PROCOPT=AP is specified but exact combination of GET/ISRT/REPL/DLET etc. not determinable from PSB source alone
- ? Business purpose of DBPAUTP0 database and PAUT segments
  - Context: Names suggest Payment Authorization (PAUT) summary/detail but no descriptive comments or context provided
