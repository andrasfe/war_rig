# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-25 18:33:25.869426

## Purpose

This PSB defines the program specification block for the DLIGSAMP COBOL program. It specifies a DL/I database PCB named PAUTBPCB for DBD DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14, including access to root segment PAUTSUM0 and dependent segment PAUTDTL1. It also defines two GSAM PCBs for datasets PASFLDBD and PADFLDBD with PROCOPT=LS.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS DL/I hierarchical database accessed via PAUTBPCB PCB with PROCOPT=GOTP and KEYLEN=14 |
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) in DBPAUTP0 database |
| PAUTDTL1 | IOType.IMS_SEGMENT | Dependent segment (PARENT=PAUTSUM0) in DBPAUTP0 database |
| PASFLDBD | IOType.FILE_SEQUENTIAL | GSAM dataset accessed via PCB with PROCOPT=LS |
| PADFLDBD | IOType.FILE_SEQUENTIAL | GSAM dataset accessed via PCB with PROCOPT=LS |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFLDBD | IOType.FILE_SEQUENTIAL | GSAM dataset potentially updated via PROCOPT=LS access |
| PADFLDBD | IOType.FILE_SEQUENTIAL | GSAM dataset potentially updated via PROCOPT=LS access |

## Open Questions

- ? What is the business purpose of DBPAUTP0, PAUTSUM0, and PAUTDTL1?
  - Context: PSB defines structure but provides no descriptive comments on data content or business use
- ? What data is stored in GSAM datasets PASFLDBD and PADFLDBD?
  - Context: Only DBDNAME and PROCOPT provided; no field or record descriptions
- ? What are the exact IMS PROCOPT values GOTP and LS?
  - Context: Non-standard or context-specific PROCOPT codes referenced
- ? Is this PSB used in batch or online IMS regions?
  - Context: PSBNAME=DLIGSAMP with LANG=COBOL but no indication of transaction or batch usage
