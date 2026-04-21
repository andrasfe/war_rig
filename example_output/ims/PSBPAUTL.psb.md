# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-04-21 13:44:39.795024

## Purpose

This PSB (Program Specification Block) defines the database access characteristics for an IMS (Information Management System) application. It specifies the database (DBPAUTP0), the processing options (PROCOPT=L), key length, and the segments (PAUTSUM0, PAUTDTL1) that the program is sensitive to. The PSB is generated for ASSEMBLY language.

## Open Questions

- ? What is the purpose of the 'L' PROCOPT?
  - Context: The meaning of 'L' for PROCOPT is not immediately clear from the code itself.
