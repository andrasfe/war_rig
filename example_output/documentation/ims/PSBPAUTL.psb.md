# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-30 19:41:31.284204

## Purpose

This file is an IMS PSB (Program Specification Block) definition that generates PSBPAUTL in Assembler language for accessing the DBPAUTP0 database. It defines a single PCB named PAUTLPCB with TYPE=DB, PROCOPT=L (locate/browse only), and KEYLEN=14. The PCB includes SENSSEG entries for root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0).

**Business Context**: Defines IMS database access interface for PAUTL application, likely a utility handling summary (PAUTSUM0) and detail (PAUTDTL1) authorization or payment data based on naming conventions.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) accessible via PAUTLPCB in DBPAUTP0 database |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment (PARENT=PAUTSUM0) accessible via PAUTLPCB in DBPAUTP0 database |
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS database accessed via PAUTLPCB PCB |

## Business Rules

- **BR001**: PCB access constrained to PROCOPT=L (locate mode for unqualified and qualified sequential processing, browse mode for SSA-specified processing; no update, insert, or delete allowed)
- **BR002**: Segment hierarchy defines PAUTSUM0 as root segment (PARENT=0) and PAUTDTL1 as its child (PARENT=PAUTSUM0)
