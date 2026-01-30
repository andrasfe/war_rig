# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-30 19:46:07.377835

## Purpose

This file is the source for generating the IMS Program Specification Block (PSB) named DLIGSAMP for use in a COBOL program. It defines a database PCB (PAUTBPCB) for the hierarchical IMS database DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14, providing access to root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0). It also defines two GSAM PCBs for databases PASFLDBD and PADFLDBD, both with PROCOPT=LS.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | Hierarchical IMS database accessed via PCB PAUTBPCB with segments PAUTSUM0 (root) and PAUTDTL1 |
| PASFLDBD | IOType.IMS_SEGMENT | GSAM IMS database accessed via unnamed PCB |
| PADFLDBD | IOType.IMS_SEGMENT | GSAM IMS database accessed via unnamed PCB |

## Open Questions

- ? What is the business purpose of the DBPAUTP0 database and its segments PAUTSUM0 and PAUTDTL1?
  - Context: PSB defines structure and access but provides no descriptive comments on data content or business use
- ? What do the GSAM databases PASFLDBD and PADFLDBD contain?
  - Context: Names suggest sequential files (possibly summary/detail logs), but no details in PSB source
- ? What are the exact meanings of PROCOPT=GOTP for the DB PCB and PROCOPT=LS for GSAM PCBs?
  - Context: Standard IMS PROCOPT documented externally; GOTP implies GET with positioning/optimization, LS implies sequential load/browse, but specifics depend on IMS version
