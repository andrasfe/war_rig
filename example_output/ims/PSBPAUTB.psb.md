# PSBPAUTB

**File**: `ims/PSBPAUTB.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 15:12:50.372498

## Purpose

This file is an IMS Program Specification Block (PSB) definition that generates PSBPAUTB for COBOL programs. It defines a single database PCB named PAUTBPCB for the DBD DBPAUTP0 with PROCOPT=AP and KEYLEN=14, providing access to the root segment PAUTSUM0 and its child segment PAUTDTL1. The PSB is generated in COBOL language with CMPAT=YES for compatibility.

**Business Context**: Defines IMS hierarchical database access structure for PAUT-related summary (PAUTSUM0) and detail (PAUTDTL1) segments, likely supporting batch DL/I applications for business data such as purchase authorizations or similar transactional records.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) of the DBPAUTP0 IMS database accessible via PAUTBPCB |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment (PARENT=PAUTSUM0) of the DBPAUTP0 IMS database accessible via PAUTBPCB |

## Open Questions

- ? Exact meaning of PROCOPT=AP in this IMS PCB context
  - Context: Standard IMS PROCOPT values include G,I,U etc., but AP is specified without further code to clarify usage (e.g., path calls or specific authorities)
- ? Business meaning of PAUTSUM0 and PAUTDTL1 segments
  - Context: Segment names suggest summary/detail structure for 'PAUT' data, but no field-level details or application context provided
