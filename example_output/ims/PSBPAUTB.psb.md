# PSBPAUTB

**File**: `ims/PSBPAUTB.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 02:34:48.664231

## Purpose

This file is the source definition for IMS Program Specification Block (PSB) named PSBPAUTB, targeted for COBOL language applications. It defines a single database PCB named PAUTBPCB for DBD DBPAUTP0 with KEYLEN=14 and PROCOPT=AP, enabling segment retrieval (GET) and unqualified insert (ISRT append) operations. The PCB senses root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) of database DBPAUTP0, accessible for retrieval via GET operations implied by PROCOPT=AP |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment of PAUTSUM0 in database DBPAUTP0, accessible for retrieval via GET operations implied by PROCOPT=AP |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS database allowing unqualified ISRT (insert as append to last positioned parent) operations enabled by PROCOPT=AP |

## Open Questions

- ? Which specific COBOL programs or transactions use this PSB?
  - Context: PSB source defines PSBNAME=PSBPAUTB but does not list using programs (line 20)
- ? What are the exact field layouts and key fields (KEYLEN=14) for segments PAUTSUM0 and PAUTDTL1?
  - Context: PSB defines segment names and hierarchy but no field-level details (lines 18-19)
- ? What is the business purpose of the PAUT database (DBPAUTP0)?
  - Context: No descriptive comments beyond copyright/license (lines 1-16)
