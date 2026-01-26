# PAUTBUNL

**File**: `ims/PAUTBUNL.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 14:24:18.203803

## Purpose

This PSB source defines the Program Specification Block for the COBOL program PAUTBUNL (line 21). It specifies one database PCB named PAUTBPCB for DBD DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14 (line 18). Sensitive segments include root PAUTSUM0 (line 19) and child PAUTDTL1 under PAUTSUM0 (line 20).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.IMS_SEGMENT | IMS database PCB for DBPAUTP0 providing access to segments PAUTSUM0 (root) and PAUTDTL1 (child) |

## Business Rules

- **BR001**: Supports IMS DL/I retrieval calls including GU (Get Unique), GN (Get Next), and GNP (Get Next within Parent) with parentage qualification
- **BR002**: Concatenated key length for unqualified segment search arguments (SSAs) is 14 bytes

## Open Questions

- ? Specific field layouts and keys for segments PAUTSUM0 and PAUTDTL1
  - Context: PSB defines segments but not field-level details; requires DBD or copybook source
- ? Business purpose of PAUTBUNL program (e.g., unload utility for PAUTP0 database)
  - Context: Inferred from name (PAUT=Payment Automation Utility?, BUNL=Bundle/Unload) but not explicit in PSB
