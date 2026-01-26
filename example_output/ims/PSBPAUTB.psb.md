# PSBPAUTB

**File**: `ims/PSBPAUTB.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 14:24:17.627872

## Purpose

This file is an IMS PSB source definition that generates the Program Specification Block PSBPAUTB for a COBOL program. It defines the single PCB PAUTBPCB for database DBDNAME=DBPAUTP0 with PROCOPT=AP and KEYLEN=14. The PCB includes sensegs PAUTSUM0 as root segment and PAUTDTL1 as child of PAUTSUM0.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.OTHER | IMS PCB TYPE=DB for DBDNAME=DBPAUTP0, PROCOPT=AP, KEYLEN=14 serving as the interface for DL/I calls to access defined segments |
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) accessible via PAUTBPCB |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment (PARENT=PAUTSUM0) accessible via PAUTBPCB |

## Business Rules

- **BR001**: PCB PAUTBPCB configured with PROCOPT=AP defining the allowed DL/I processing options for the database
- **BR002**: PCB PAUTBPCB configured with KEYLEN=14 for segment search arguments
- **BR003**: PAUTSUM0 defined as root segment with no parent
- **BR004**: PAUTDTL1 defined as child segment of PAUTSUM0
