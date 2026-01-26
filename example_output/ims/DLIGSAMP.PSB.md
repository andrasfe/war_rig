# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 02:33:52.778949

## Purpose

The DLIGSAMP.PSB is an IMS Program Specification Block (PSB) source file that defines database access structures for a COBOL DL/I application. It specifies a primary database PCB named PAUTBPCB for the DBPAUTP0 database with read-only GET processing on root segment PAUTSUM0 and child segment PAUTDTL1, plus two GSAM PCBs for sequential locate access to PASFLDBD and PADFLDBD datasets. The PSB is generated for COBOL language use without compatibility mode.

**Business Context**: Sample IMS DL/I application demonstrating hierarchical database and GSAM dataset access, licensed under Apache 2.0 by Amazon.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS hierarchical database accessed via PAUTBPCB with segments PAUTSUM0 (root, PARENT=0) and PAUTDTL1 (PARENT=PAUTSUM0) |
| PASFLDBD | IOType.OTHER | GSAM dataset accessed via unnamed PCB with Locate Sequential option |
| PADFLDBD | IOType.OTHER | GSAM dataset accessed via unnamed PCB with Locate Sequential option |

## Business Rules

- **BR001**: Permitted DL/I calls on DBPAUTP0 database are restricted to read-only GET operations
- **BR002**: GSAM datasets PASFLDBD and PADFLDBD support only Locate Sequential access

## Open Questions

- ? What specific fields are accessed within segments PAUTSUM0, PAUTDTL1, and the GSAM datasets?
  - Context: PSB source defines segments and datasets at high level but does not include field-level layouts or SSA specifications
- ? Is this PSB used in batch DL/I, BMP, MPP, or IFP (Fast Path) context?
  - Context: No transaction PCB (TPCB) defined; GSAM PCBs suggest possible Fast Path but PROCOPT=LS is non-standard for IFP
