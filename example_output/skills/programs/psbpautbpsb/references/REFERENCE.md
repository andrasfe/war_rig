# PSBPAUTB - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** PSBPAUTB
- **File Name:** ims/PSBPAUTB.psb
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:33:44.020090

## Purpose

**Summary:** Defines the Program Specification Block (PSB) PSBPAUTB for a COBOL application program using IMS DL/I calls. Specifies a single database PCB named PAUTBPCB for accessing DBD DBPAUTP0 with PROCOPT=AP and KEYLEN=14. Includes root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 under PAUTSUM0.

**Business Context:** IMS database access configuration for batch DL/I programs interacting with the PAUTP0 database hierarchy
**Program Type:** BATCH

## Inputs

### PAUTSUM0

- **Type:** IMS_SEGMENT
- **Description:** Root segment (PARENT=0) accessible via PAUTBPCB PCB

### PAUTDTL1

- **Type:** IMS_SEGMENT
- **Description:** Child segment with PARENT=PAUTSUM0 accessible via PAUTBPCB PCB

## Data Flow
