# PSBPAUTB

**File**: `ims/PSBPAUTB.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 17:40:21.924538

## Purpose

This PSB source file defines a single IMS database PCB named PAUTBPCB for accessing DBDNAME=DBPAUTP0 with PROCOPT=AP and KEYLEN=14. It senses the root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0), enabling qualified and unqualified DL/I calls to these segments. The PSB is generated for COBOL language with PSBNAME=PSBPAUTB.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) sensed for access via DL/I calls such as GU/GN |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment under PAUTSUM0 sensed for access via DL/I calls such as GU/GN |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | New root segments insertable via ISRT after positioning |
| PAUTDTL1 | IOType.IMS_SEGMENT | New child segments insertable via ISRT under PAUTSUM0 after positioning |

## Business Rules

- **BR001**: PROCOPT=AP governs segment access, permitting positioning (GU/GN) and insert (ISRT) DL/I calls but prohibiting update (CHG) or delete (DLT)

## Open Questions

- ? What are the exact segment layouts and key fields?
  - Context: PSB defines access but not field-level details; these are in DBD/PSBGEN output or application copybooks
- ? Which application programs use this PSB?
  - Context: PSB source does not specify callers
