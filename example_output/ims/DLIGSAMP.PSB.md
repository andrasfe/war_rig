# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 15:17:26.169793

## Purpose

This file is the source for the IMS Program Specification Block (PSB) named DLIGSAMP, targeted for a COBOL language application program. It defines one database PCB (PAUTBPCB) providing access to the hierarchical IMS database DBDNAME=DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14, including root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0). It also defines two GSAM PCBs for read access (PROCOPT=LS) to DBDNAME=PASFLDBD and DBDNAME=PADFLDBD. The PSB is generated with PRINT NOGEN, no compatibility mode (CMPAT=NO), for use in IMS DL/I calls from the DLIGSAMP COBOL program.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS hierarchical database accessed via PAUTBPCB with segments PAUTSUM0 (root) and PAUTDTL1 (child), PROCOPT=GOTP, KEYLEN=14 |
| PASFLDBD | IOType.FILE_SEQUENTIAL | GSAM dataset accessed via unnamed PCB with PROCOPT=LS |
| PADFLDBD | IOType.FILE_SEQUENTIAL | GSAM dataset accessed via unnamed PCB with PROCOPT=LS |

## Open Questions

- ? What is the specific business purpose or application context of the DLIGSAMP program and its accessed IMS resources (DBPAUTP0, PASFLDBD, PADFLDBD)?
  - Context: The PSB defines technical access structures but contains no descriptive comments, segment field details, or business logic indications.
- ? Is this PSB intended for batch (DLI batch) or online IMS regions (e.g., MPP, BMP, IFP)?
  - Context: PROCOPT values (GOTP, LS) support both, but no explicit indication of transaction IDs or online-specific features.
