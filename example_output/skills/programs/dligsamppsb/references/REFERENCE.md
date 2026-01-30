# DLIGSAMP - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** DLIGSAMP
- **File Name:** ims/DLIGSAMP.PSB
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:46:07.377835

## Purpose

**Summary:** This file is the source for generating the IMS Program Specification Block (PSB) named DLIGSAMP for use in a COBOL program. It defines a database PCB (PAUTBPCB) for the hierarchical IMS database DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14, providing access to root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0). It also defines two GSAM PCBs for databases PASFLDBD and PADFLDBD, both with PROCOPT=LS.

**Business Context:** None
**Program Type:** UTILITY

## Inputs

### DBPAUTP0

- **Type:** IMS_SEGMENT
- **Description:** Hierarchical IMS database accessed via PCB PAUTBPCB with segments PAUTSUM0 (root) and PAUTDTL1

### PASFLDBD

- **Type:** IMS_SEGMENT
- **Description:** GSAM IMS database accessed via unnamed PCB

### PADFLDBD

- **Type:** IMS_SEGMENT
- **Description:** GSAM IMS database accessed via unnamed PCB

## Data Flow
