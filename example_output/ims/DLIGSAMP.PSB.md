# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 02:41:29.510044

## Purpose

This file is the source input for the IMS PSBGEN utility to generate the Program Specification Block (PSB) named DLIGSAMP for a COBOL language program. It defines one database PCB (PAUTBPCB) for the IMS hierarchical database DBDNAME=DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14, including sensitive segments PAUTSUM0 (root, PARENT=0) and PAUTDTL1 (PARENT=PAUTSUM0). It also defines two GSAM PCBs for sequential datasets PASFLDBD and PADFLDBD, each with PROCOPT=LS.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS hierarchical database accessed via PCB named PAUTBPCB with PROCOPT=GOTP and KEYLEN=14; sensitive segments are PAUTSUM0 (PARENT=0) and PAUTDTL1 (PARENT=PAUTSUM0) |
| PASFLDBD | IOType.FILE_SEQUENTIAL | GSAM dataset accessed via unnamed PCB with PROCOPT=LS |
| PADFLDBD | IOType.FILE_SEQUENTIAL | GSAM dataset accessed via unnamed PCB with PROCOPT=LS |
