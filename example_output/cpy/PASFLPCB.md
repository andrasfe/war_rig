# PASFLPCB

**File**: `cpy/PASFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-25 18:33:38.308749

## Purpose

This copybook defines the PASFLPCB 01-level group item, which represents the IMS Program Communication Block (PCB) for the PASFL database. It includes fields for database name, segment level, PCB status, processing options, segment name, key feedback name offset, number of sensitive segments, and the key feedback buffer. This structure is used in IMS DL/I programs for database navigation and segment access.

**Business Context**: IMS DL/I hierarchical database access using the PASFL database definition (DBD)

## Open Questions

- ? What specific business application uses the PASFL database?
  - Context: The copybook defines a standard IMS PCB for PASFL DBD but lacks context on the business domain or using programs.
