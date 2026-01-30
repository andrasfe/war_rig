# PSBPAUTB

**File**: `ims/PSBPAUTB.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-30 19:33:44.020090

## Purpose

Defines the Program Specification Block (PSB) PSBPAUTB for a COBOL application program using IMS DL/I calls. Specifies a single database PCB named PAUTBPCB for accessing DBD DBPAUTP0 with PROCOPT=AP and KEYLEN=14. Includes root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 under PAUTSUM0.

**Business Context**: IMS database access configuration for batch DL/I programs interacting with the PAUTP0 database hierarchy

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) accessible via PAUTBPCB PCB |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment with PARENT=PAUTSUM0 accessible via PAUTBPCB PCB |

## Open Questions

- ? What is the exact meaning of PROCOPT=AP in this PSB PCB definition?
  - Context: PROCOPT=AP is specified but not a standard single PROCOPT value like G, I, R; may indicate combined options for insert/retrieve or application-specific processing
- ? What application programs use this PSB, and what business data do PAUTSUM0 and PAUTDTL1 represent?
  - Context: PSB is infrastructure; no calling programs or segment field details provided in source
