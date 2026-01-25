# PSBPAUTB

**File**: `ims/PSBPAUTB.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-25 18:37:47.645628

## Purpose

Defines the IMS Program Specification Block (PSB) named PSBPAUTB for a COBOL application program. Specifies a single database PCB (PAUTBPCB) for access to the DBPAUTP0 database with PROCOPT=AP and KEYLEN=14. Sensitizes root segment PAUTSUM0 and child segment PAUTDTL1.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.IMS_SEGMENT | IMS DL/I database DBPAUTP0 with root segment PAUTSUM0 (PARENT=0) and dependent segment PAUTDTL1 (PARENT=PAUTSUM0) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.IMS_SEGMENT | IMS DL/I database DBPAUTP0 for write operations enabled by PROCOPT=AP (e.g., append/insert new segments) |

## Open Questions

- ? What exact DL/I call types (e.g., ISRT, REPL, DLT) are permitted by PROCOPT=AP?
  - Context: Source code specifies PROCOPT=AP but does not detail permitted operations; requires IMS DL/I reference.
