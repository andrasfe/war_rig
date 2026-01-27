# PAUTBUNL

**File**: `ims/PAUTBUNL.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 02:41:39.309305

## Purpose

This is an IMS Program Specification Block (PSB) definition file for the COBOL program PAUTBUNL. It defines a single database PCB named PAUTBPCB providing access to IMS database DBDNAME=DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14. The PCB includes two sensegments: PAUTSUM0 as the root segment (PARENT=0) and PAUTDTL1 as a child segment of PAUTSUM0.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.IMS_SEGMENT | IMS database PCB for DBDNAME=DBPAUTP0 with PROCOPT=GOTP (get next in TP mode) and KEYLEN=14; includes senseg PAUTSUM0 (root) and PAUTDTL1 (child of PAUTSUM0) |

## Open Questions

- ? What are the field layouts and key fields for segments PAUTSUM0 and PAUTDTL1?
  - Context: PSB defines segment names and hierarchy but no field-level details
- ? What is the exact business function of program PAUTBUNL using this PSB?
  - Context: PSB provides only database access specs, no functional description
