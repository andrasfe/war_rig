# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: OTHER
**Analyzed**: 2026-01-27 23:05:45.249565

## Purpose

This PSB (Program Specification Block) defines the database access characteristics for an IMS application program. It specifies the PCBs (Program Communication Blocks) that the program will use to interact with IMS databases, including DB PCBs for DBPAUTP0 and GSAM PCBs for PASFLDBD and PADFLDBD.

## Open Questions

- ? What is the specific purpose of the PAUTSUM0 and PAUTDTL1 segments?
  - Context: The code defines these segments as SENSEG but doesn't provide enough context to understand their role in the database structure.
