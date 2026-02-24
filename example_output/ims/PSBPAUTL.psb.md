# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-24 17:38:51.764370

## Purpose

This PSB (Program Specification Block) defines the program's interface to the IMS database DBPAUTP0. It specifies the segments (PAUTSUM0, PAUTDTL1) that the program is allowed to access and the type of access (read-only).

## Business Rules

- **BR001**: The program has read-only access to the DBPAUTP0 database.

## Open Questions

- ? What is the purpose of the PAUTSUM0 and PAUTDTL1 segments?
  - Context: The PSB defines these segments, but their specific function is not clear from this file alone.
