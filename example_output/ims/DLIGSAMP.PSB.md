# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-24 03:53:28.634551

## Purpose

This is the source file for generating an IMS Program Specification Block (PSB) named DLIGSAMP for a COBOL program (LANG=COBOL). It defines a primary database PCB (PAUTBPCB) for the IMS hierarchical database DBDNAME=DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14, enabling access to root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0). It also defines two GSAM PCBs for DBDNAME=PASFLDBD and PADFLDBD, both with PROCOPT=LS.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS hierarchical database PCB (PAUTBPCB) with PROCOPT=GOTP, KEYLEN=14, providing access to segments PAUTSUM0 and PAUTDTL1 |
| PASFLDBD | IOType.OTHER | IMS GSAM database PCB with PROCOPT=LS |
| PADFLDBD | IOType.OTHER | IMS GSAM database PCB with PROCOPT=LS |

## Open Questions

- ? What is the business purpose of the databases DBPAUTP0 (segments PAUTSUM0, PAUTDTL1), PASFLDBD, and PADFLDBD?
  - Context: PSB source provides no descriptive comments or context beyond DBDNAMEs and segment names.
- ? Is this PSB intended for batch DL/I, IMS online (MPP/BMP), or other execution modes?
  - Context: PROCOPT=GOTP and LS provide access intent hints but do not specify program execution context.
- ? What specific DL/I call types (GU, GN, ISRT, etc.) are authorized by these PROCOPT settings?
  - Context: PSB defines PCB-level PROCOPT but not per-segment or exact call permissions.
