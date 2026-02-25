# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-25 15:31:46.361586

## Purpose

This file is a Program Specification Block (PSB) source definition for the IMS DL/I application program DLIGSAMP, generated for COBOL language with no compatibility mode. It defines a database PCB named PAUTBPCB for the hierarchical IMS database DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14, including root segment PAUTSUM0 and child segment PAUTDTL1. It also defines two GSAM PCBs for sequential access to databases PASFLDBD and PADFLDBD with PROCOPT=LS.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS hierarchical database accessed via PAUTBPCB PCB with segments PAUTSUM0 (PARENT=0) and PAUTDTL1 (PARENT=PAUTSUM0) |
| PASFLDBD | IOType.FILE_SEQUENTIAL | GSAM database PCB with PROCOPT=LS for low-sequence access |
| PADFLDBD | IOType.FILE_SEQUENTIAL | GSAM database PCB with PROCOPT=LS for low-sequence access |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| DLIGSAMP | IOType.OTHER | Generated PSB load module for use by the DLIGSAMP COBOL application program |

## Open Questions

- ? What is the specific business process served by the DLIGSAMP program using this PSB?
  - Context: The file contains only IMS PSBGEN macros, license header, and no descriptive comments on business logic or application purpose.
