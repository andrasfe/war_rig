# PAUTBUNL - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** PAUTBUNL
- **File Name:** ims/PAUTBUNL.PSB
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:42:22.408833

## Purpose

**Summary:** This file defines an IMS Program Specification Block (PSB) named PAUTBUNL for a COBOL language program. It specifies a single PCB named PAUTBPCB providing access to the IMS database DBDNAME=DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14. The PCB defines two sensegments: PAUTSUM0 as the root segment (PARENT=0) and PAUTDTL1 as a child segment under PAUTSUM0.

**Business Context:** None
**Program Type:** BATCH

## Inputs

### DBPAUTP0

- **Type:** IMS_SEGMENT
- **Description:** IMS database referenced by the PCB, accessible via segments PAUTSUM0 (root) and PAUTDTL1 (child)

## Data Flow
