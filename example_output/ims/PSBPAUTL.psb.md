# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 02:41:26.528725

## Purpose

This file defines an IMS Program Specification Block (PSB) named PSBPAUTL for database access. It specifies a single Database PCB named PAUTLPCB targeting the database DBDNAME=DBPAUTP0 with PROCOPT=L and KEYLEN=14. The PCB defines access to two segments: root segment PAUTSUM0 (PARENT=0) and dependent segment PAUTDTL1 under PAUTSUM0.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTLPCB | IOType.IMS_SEGMENT | IMS Database PCB providing access to DBPAUTP0 database, including segments PAUTSUM0 (root) and PAUTDTL1 (child of PAUTSUM0) |

## Open Questions

- ? What is the exact meaning of PROCOPT=L in this IMS PSB context?
  - Context: PROCOPT=L is specified but standard IMS PROCOPT values are typically G, I, R, D, etc.; L is not clearly defined from the source alone.
- ? What application programs or transactions use this PSB?
  - Context: PSB is referenced by IMS application programs, but no calling context is defined in this file.
- ? What are the data fields in segments PAUTSUM0 and PAUTDTL1?
  - Context: PSB defines segment names but not field layouts, which are in the DBD.
