# PAUTBUNL

**File**: `ims/PAUTBUNL.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-25 18:33:15.174451

## Purpose

Defines the Program Specification Block (PSB) named PAUTBUNL for a COBOL program. Specifies a single database PCB (PAUTBPCB) for accessing IMS database DBDNAME=DBPAUTP0 using PROCOPT=GOTP with KEYLEN=14. Declares two sensitive segments: PAUTSUM0 (root, PARENT=0) and PAUTDTL1 (child of PAUTSUM0).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root sensitive segment (PARENT=0) accessible via PAUTBPCB PCB in DBPAUTP0 database |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child sensitive segment (PARENT=PAUTSUM0) accessible via PAUTBPCB PCB in DBPAUTP0 database |

## Business Rules

- **BR001**: Database access restricted to sensitive segments PAUTSUM0 and PAUTDTL1 under PCB PAUTBPCB
- **BR002**: Processing option set to GOTP (Get Unique Positioned) with 14-byte key length

## Open Questions

- ? What is the business purpose of the PAUTBUNL program and DBPAUTP0 database?
  - Context: PSB source defines only access structure, not business logic or data semantics
- ? What are the field layouts and key fields (KEYLEN=14) for segments PAUTSUM0 and PAUTDTL1?
  - Context: PSB does not include segment or field definitions
- ? Is this PSB used in batch, BMP, MPP, or IFP IMS region?
  - Context: No I/O PCB defined; only DB PCB with GOTP suggests non-transactional (batch/BMP) but cannot confirm
