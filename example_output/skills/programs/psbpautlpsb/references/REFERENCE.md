# PSBPAUTL - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** PSBPAUTL
- **File Name:** ims/PSBPAUTL.psb
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:41:31.284204

## Purpose

**Summary:** This file is an IMS PSB (Program Specification Block) definition that generates PSBPAUTL in Assembler language for accessing the DBPAUTP0 database. It defines a single PCB named PAUTLPCB with TYPE=DB, PROCOPT=L (locate/browse only), and KEYLEN=14. The PCB includes SENSSEG entries for root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0).

**Business Context:** Defines IMS database access interface for PAUTL application, likely a utility handling summary (PAUTSUM0) and detail (PAUTDTL1) authorization or payment data based on naming conventions.
**Program Type:** UTILITY

## Inputs

### PAUTSUM0

- **Type:** IMS_SEGMENT
- **Description:** Root segment (PARENT=0) accessible via PAUTLPCB in DBPAUTP0 database

### PAUTDTL1

- **Type:** IMS_SEGMENT
- **Description:** Child segment (PARENT=PAUTSUM0) accessible via PAUTLPCB in DBPAUTP0 database

### DBPAUTP0

- **Type:** IMS_SEGMENT
- **Description:** IMS database accessed via PAUTLPCB PCB

## Business Rules

### BR001

**Description:** PCB access constrained to PROCOPT=L (locate mode for unqualified and qualified sequential processing, browse mode for SSA-specified processing; no update, insert, or delete allowed)

**Logic:** PROCOPT=L specified in PCB definition limits programs using this PSB to read-only operations on the database

### BR002

**Description:** Segment hierarchy defines PAUTSUM0 as root segment (PARENT=0) and PAUTDTL1 as its child (PARENT=PAUTSUM0)

**Logic:** SENSEG statements establish the logical parent-child relationships for database navigation

**Conditions:**
- `PARENT=0 for root`
- `PARENT=PAUTSUM0 for child`

## Data Flow
