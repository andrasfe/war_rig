# PSBPAUTB

**File**: `ims/PSBPAUTB.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-24 03:59:57.386110

## Purpose

This file defines the IMS Program Specification Block (PSB) named PSBPAUTB for a COBOL application. It specifies a single Database PCB named PAUTBPCB accessing IMS database DBDNAME=DBPAUTP0 with PROCOPT=AP and KEYLEN=14. The PCB senses root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) sensed by PCB PAUTBPCB in database DBPAUTP0 |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment (PARENT=PAUTSUM0) sensed by PCB PAUTBPCB in database DBPAUTP0 |
| DBPAUTP0 | IOType.OTHER | IMS database (DBDNAME) accessed via PCB PAUTBPCB |

## Open Questions

- ? What is the specific business purpose of database DBPAUTP0 and segments PAUTSUM0/PAUTDTL1?
  - Context: File contains only PSB syntax with no comments or descriptive prose explaining application context.
- ? What does PROCOPT=AP precisely authorize (e.g., read-only, update capabilities)?
  - Context: Standard IMS PROCOPT codes documented externally; file does not specify.
