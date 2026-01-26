# PASFLPCB

**File**: `cpy/PASFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 14:22:50.167330

## Purpose

This COBOL copybook defines the PASFLPCB record structure, which is the Program Communication Block (PCB) for the PASFL IMS database. It specifies fields for DBD name (line 18), segment level (line 19), PCB status (line 20), processing options (line 21), filler (line 22), segment name (line 23), key feedback name (line 24), number of sensitive segments (line 25), and key feedback area (line 26). This structure is used in IMS DL/I application programs to interface with the PASFL database for call statements like GU, GN, etc.

**Business Context**: IMS DL/I database access for the PASFL database

## Open Questions

- ? Specific programs or sections where this copybook is included
  - Context: Copybook defines structure but does not indicate including programs or exact placement (e.g., WORKING-STORAGE or LINKAGE)
- ? Length of key feedback area
  - Context: PASFL-KEYFB defined as PIC X(100) (line 26), but actual length may vary based on database keys
