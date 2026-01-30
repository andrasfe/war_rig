# PAUTBPCB

**File**: `cpy/PAUTBPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-30 19:48:44.774204

## Purpose

This copybook defines the data structure PAUTBPCB (line 17), which is the Program Communication Block (PCB) for the PAUT IMS database. It includes fields for database name (PAUT-DBDNAME, line 18), current segment level (PAUT-SEG-LEVEL, line 19), PCB status code (PAUT-PCB-STATUS, line 20), processing options (PAUT-PCB-PROCOPT, line 21), segment name (PAUT-SEG-NAME, line 23), key feedback name offset (PAUT-KEYFB-NAME, line 24), number of sensitive segments (PAUT-NUM-SENSEGS, line 25), and a 255-byte key feedback buffer (PAUT-KEYFB, line 26). This structure is used by IMS DL/I programs to manage database calls and retrieve status and positioning information.

**Business Context**: IMS DL/I hierarchical database navigation and access for the PAUT database

## Paragraphs/Procedures

### ~~PAUTBPCB~~ (Dead Code)
*Record layout 'PAUTBPCB' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| PAUTBPCB | record_layout | 1 | Record layout 'PAUTBPCB' is never used by any program |

## Open Questions

- ? Which specific IMS programs include this copybook?
  - Context: The copybook file does not reference including programs
