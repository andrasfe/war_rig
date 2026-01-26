# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 02:33:26.241011

## Purpose

This is an IMS Program Specification Block (PSB) definition file that generates PSBPAUTL for assembler language. It defines a single database PCB named PAUTLPCB accessing DBDNAME=DBPAUTP0 with PROCOPT=L (language interface) and KEYLEN=14. The PCB senses root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) sensed in PCB PAUTLPCB for database DBPAUTP0 |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment (PARENT=PAUTSUM0) sensed in PCB PAUTLPCB for database DBPAUTP0 |

## Business Rules

- **BR001**: Defines IMS segment hierarchy for PCB access: PAUTSUM0 as root segment (PARENT=0), PAUTDTL1 as dependent child segment of PAUTSUM0

## Open Questions

- ? What IMS application programs use this PSB (PSBPAUTL)?
  - Context: PSB definition does not specify calling programs
- ? Specific business purpose of DBPAUTP0 database and segments PAUTSUM0/PAUTDTL1?
  - Context: Names suggest Payment Authority Utility (PAUT), but no descriptive comments
