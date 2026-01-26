# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 14:24:07.322896

## Purpose

This file defines the IMS Program Specification Block (PSB) named PSBPAUTL for assembly language (LANG=ASSEM). It specifies a single database PCB named PAUTLPCB for accessing the DBPAUTP0 database in Learn/Browse mode only (PROCOPT=L) with key length 14. The PCB includes the root segment PAUTSUM0 (PARENT=0) and its child segment PAUTDTL1 (PARENT=PAUTSUM0).

**Business Context**: IMS database access control block for PAUTL utility, enabling qualified segment search on PAUT database hierarchy.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS database accessed via PAUTLPCB PCB |
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) in DBPAUTP0 |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment under PAUTSUM0 in DBPAUTP0 |

## Business Rules

- **BR001**: Access restricted to Learn/Browse mode only
- **BR002**: Defines segment hierarchy with PAUTSUM0 as root and PAUTDTL1 as dependent child

## Open Questions

- ? Specific fields or keys accessed in segments
  - Context: Only KEYLEN=14 specified; no field-level details in PSB
- ? Application programs referencing this PSB
  - Context: PSB defines access but does not list callers
