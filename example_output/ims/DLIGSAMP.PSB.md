# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 17:39:00.243223

## Purpose

The DLIGSAMP.PSB defines the IMS Program Specification Block (PSB) for the COBOL program DLIGSAMP. It specifies three Program Communication Blocks (PCBs): PAUTBPCB for hierarchical database DBPAUTP0 with segments PAUTSUM0 (root) and PAUTDTL1 (child under PROCOPT=GOTP, KEYLEN=14), and two unnamed GSAM PCBs for datasets PASFLDBD and PADFLDBD (both PROCOPT=LS). The PSB is generated with PRINT NOGEN and CMPAT=NO.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | Hierarchical IMS database accessed via named PCB PAUTBPCB (TYPE=DB, PROCOPT=GOTP, KEYLEN=14) with qualified segments PAUTSUM0 (PARENT=0) and PAUTDTL1 (PARENT=PAUTSUM0) |
| PASFLDBD | IOType.IMS_SEGMENT | GSAM dataset accessed via unnamed PCB (TYPE=GSAM, PROCOPT=LS) |
| PADFLDBD | IOType.IMS_SEGMENT | GSAM dataset accessed via unnamed PCB (TYPE=GSAM, PROCOPT=LS) |

## Open Questions

- ? What is the business purpose of databases DBPAUTP0 (segments PAUTSUM0/PAUTDTL1), PASFLDBD, and PADFLDBD?
  - Context: PSB source provides only structural access definitions with no descriptive comments or context
- ? Are there any write/update operations possible via these PCBs?
  - Context: PROCOPT values (GOTP, LS) suggest read-only access (Get operations), but cannot confirm without program source
