# PAUTBPCB

**File**: `cpy/PAUTBPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 17:42:09.753247

## Purpose

This copybook defines the level-01 data structure PAUTBPCB, which matches the standard IMS Database Program Communication Block (DBPCB) layout for DL/I database calls. It includes fields for DBD name (line 18), segment level (line 19), PCB status code (line 20), processing options (line 21), segment name (line 23), key feedback offset (line 24), number of sensitive segments (line 25), and key feedback buffer (line 26). Used in IMS-enabled COBOL programs to receive status and positioning information from DL/I calls.

**Business Context**: IMS DL/I database access and navigation

## Open Questions

- ? Specific programs or PSBs that use this PAUTBPCB copybook?
  - Context: Copybook defines structure but does not indicate usage context or including programs.
