# PAUTBUNL

**File**: `ims/PAUTBUNL.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-25 15:22:41.101203

## Purpose

This file defines an IMS Program Specification Block (PSB) named PAUTBUNL for a COBOL program. It specifies a Database PCB (PAUTBPCB) for accessing the IMS database DBDNAME=DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14. The PCB includes two sensitive segments: PAUTSUM0 as the root (PARENT=0) and PAUTDTL1 as its child (PARENT=PAUTSUM0).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS database referenced by the PAUTBPCB PCB, with sensitive segments PAUTSUM0 (root) and PAUTDTL1 (child of PAUTSUM0) |

## Open Questions

- ? What is the business purpose of the DBPAUTP0 database and its segments PAUTSUM0/PAUTDTL1?
  - Context: PSB defines access structure but provides no description of data content or usage
- ? What does the PAUTBUNL COBOL program perform using this PSB?
  - Context: PSB is a specification file only; no program logic present
