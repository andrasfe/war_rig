# PAUTBPCB

**File**: `cpy/PAUTBPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 14:24:31.886625

## Purpose

This copybook defines the PAUTBPCB level-01 group structure, which is the IMS Database Program Communication Block (PCB) for the PAUT database. It includes standard IMS DBPCB fields such as DBD name, segment level and name, PCB status and processing options, number of sensitive segments, and a key feedback buffer area. This structure is used in COBOL programs (typically in the LINKAGE SECTION) to interface with IMS DL/I calls for database navigation and access.

**Business Context**: IMS DL/I hierarchical database access for applications processing PAUT database segments

## Open Questions

- ? Specific programs or PSBs that include this PCB
  - Context: Copybook does not reference including programs or PSB definitions
- ? Exact length and offset details of PAUT-KEYFB
  - Context: Field is PIC X(255), but actual key feedback length depends on PSB and segment keys
