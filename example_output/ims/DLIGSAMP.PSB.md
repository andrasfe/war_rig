# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: OTHER
**Analyzed**: 2026-01-28 14:52:49.593060

## Purpose

This PSB (Program Specification Block) defines the database access characteristics for an IMS application program. It specifies the PCBs (Program Communication Blocks) that the program will use to interact with IMS databases, including DB PCBs for DBPAUTP0 and GSAM PCBs for PASFLDBD and PADFLDBD.

## Open Questions

- ? What is the specific purpose of the DBPAUTP0, PASFLDBD, and PADFLDBD databases?
  - Context: The DBDNAME values are provided, but the function of the databases is not clear from this PSB.
- ? What is the significance of the PROCOPT values (GOTP, LS) for each PCB?
  - Context: The PROCOPT values are defined, but their impact on program behavior is unclear.
