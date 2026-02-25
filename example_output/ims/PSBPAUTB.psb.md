# PSBPAUTB

**File**: `ims/PSBPAUTB.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-25 15:30:16.875951

## Purpose

This file defines an IMS Program Specification Block (PSB) named PSBPAUTB for COBOL-language application programs. It specifies a single database PCB named PAUTBPCB that accesses the DBPAUTP0 database with application-controlled processing (PROCOPT=AP) and a key length of 14 bytes (KEYLEN=14). The PCB senses the root segment PAUTSUM0 (PARENT=0) and its child segment PAUTDTL1 (PARENT=PAUTSUM0).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS database accessed via PCB PAUTBPCB, with accessible segments PAUTSUM0 (root) and PAUTDTL1 (child of PAUTSUM0) |

## Business Rules

- **BR001**: Database PCB PAUTBPCB uses PROCOPT=AP for application-controlled processing and KEYLEN=14 for the search key length
- **BR002**: SENSEG definitions limit access to PAUTSUM0 as root segment (PARENT=0) and PAUTDTL1 as child segment under PAUTSUM0

## Open Questions

- ? What business data do the PAUTSUM0 and PAUTDTL1 segments represent?
  - Context: Segment names suggest summary and detail data (possibly automation or audit), but no field-level details provided in PSB
- ? What IMS application programs (COBOL) use this PSB?
  - Context: PSBGEN specifies LANG=COBOL and PSBNAME=PSBPAUTB, but no calling programs listed
