# PAUTBUNL

**File**: `ims/PAUTBUNL.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 15:15:03.203687

## Purpose

This PSB source file defines the Program Specification Block for the COBOL program PAUTBUNL, specifying IMS database access via a single PCB named PAUTBPCB to the DBPAUTP0 database with PROCOPT=GOTP and KEYLEN=14. It declares two sensed segments: PAUTSUM0 as the root segment (PARENT=0) and PAUTDTL1 as a child segment under PAUTSUM0. The PSB is generated for COBOL language compatibility without compatibility mode.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) of the DBPAUTP0 IMS database, accessible via PAUTBPCB PCB |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment (PARENT=PAUTSUM0) of the DBPAUTP0 IMS database, accessible via PAUTBPCB PCB |

## Business Rules

- **BR001**: PCB access constrained to processing options defined by PROCOPT=GOTP, permitting Get Unique retrieval operations with positioning support
- **BR002**: Key length for DL/I segment searches limited to 14 bytes

## Open Questions

- ? What is the business purpose of the DBPAUTP0 database and PAUT segments?
  - Context: PSB defines access structure but provides no descriptive comments on data content or application use
