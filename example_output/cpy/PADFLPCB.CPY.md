# PADFLPCB

**File**: `cpy/PADFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 02:41:59.543157

## Purpose

This COBOL copybook defines the PADFLPCB group item (line 17), which is the Program Communication Block (PCB) for the PADFL IMS database. It specifies fields including database name (PADFL-DBDNAME, line 18), segment level (line 19), PCB status (line 20), processing options (line 21), segment name (line 23), key feedback name offset (line 24), number of sensitive segments (line 25), and key feedback buffer (line 26). This structure is used in IMS DL/I programs to obtain status and navigation information during database calls.

**Business Context**: IMS database access and navigation for the PADFL database, providing control block fields for segment search and status reporting (lines 17-26).

## Open Questions

- ? In which programs or PSBs is this copybook included?
  - Context: The copybook defines a data structure but does not specify usage context or including programs.
