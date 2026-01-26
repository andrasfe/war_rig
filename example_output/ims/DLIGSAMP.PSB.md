# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 14:22:11.654695

## Purpose

This file is a Program Specification Block (PSB) source definition for the DLIGSAMP COBOL program in IMS. It defines three PCBs: a database PCB (PAUTBPCB) for the hierarchical IMS database DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14, including two sensitive segments PAUTSUM0 (root) and PAUTDTL1 (child of PAUTSUM0); and two GSAM PCBs for datasets PASFLDBD and PADFLDBD with PROCOPT=LS. The PSB is generated specifically for COBOL language with PSBNAME=DLIGSAMP.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS hierarchical database accessed via PAUTBPCB PCB with PROCOPT=GOTP and KEYLEN=14 |
| PAUTSUM0 | IOType.IMS_SEGMENT | Root-level sensitive segment (PARENT=0) in DBPAUTP0 |
| PAUTDTL1 | IOType.IMS_SEGMENT | Sensitive segment dependent on parent PAUTSUM0 in DBPAUTP0 |
| PASFLDBD | IOType.FILE_SEQUENTIAL | GSAM dataset accessed via unnamed PCB with PROCOPT=LS |
| PADFLDBD | IOType.FILE_SEQUENTIAL | GSAM dataset accessed via unnamed PCB with PROCOPT=LS |

## Open Questions

- ? What is the specific business purpose of the DLIGSAMP program and the data in DBPAUTP0, PASFLDBD, PADFLDBD?
  - Context: PSB source defines access structures but contains no descriptive comments or business logic details
