# PAUTBUNL

**File**: `ims/PAUTBUNL.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 17:42:09.963326

## Purpose

This PSB source defines the Program Specification Block for COBOL program PAUTBUNL, specifying a database PCB PAUTBPCB for accessing IMS database DBPAUTP0 with PROCOPT=GOTP (GO=Get Next using SSAs + TP=IMS TM mode), KEYLEN=14. It senses root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0). The PSBGEN generates the control block for DL/I calls in the application (lines 17-22).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS database accessed via PCB PAUTBPCB |
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) sensed by the PCB |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment (PARENT=PAUTSUM0) sensed by the PCB |

## Business Rules

- **BR001**: PCB senses root segment PAUTSUM0 with no parent
- **BR002**: PCB senses detail segment PAUTDTL1 as child of PAUTSUM0
- **BR003**: PROCOPT=GOTP enables Get Next segment retrieval using SSAs in IMS TM/TP mode
