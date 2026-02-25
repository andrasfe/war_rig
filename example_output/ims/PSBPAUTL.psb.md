# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-25 15:30:33.688584

## Purpose

This file is an IMS Program Specification Block (PSB) generation control statement file that defines PSBPAUTL for assembler language programs. It specifies a single database PCB named PAUTLPCB for accessing the DBPAUTP0 database with PROCOPT=L (load/unload processing option) and a key length of 14. The PCB defines two sensitive segments: root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 under parent PAUTSUM0.

## Open Questions

- ? What is the business purpose of the DBPAUTP0 database and its segments PAUTSUM0/PAUTDTL1?
  - Context: No descriptive comments or names indicate business function beyond IMS DL/I access definitions.
- ? Which application programs use this PSB (PSBPAUTL)?
  - Context: PSB defines access for unnamed application programs; no references in this file.
