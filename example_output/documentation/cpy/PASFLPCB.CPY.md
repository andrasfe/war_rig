# PASFLPCB

**File**: `cpy/PASFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-30 19:48:15.953397

## Purpose

This COBOL copybook defines the data structure for the PASFL Program Communication Block (PCB) used in IMS DL/I application programs. It specifies fields including the database name, segment level, PCB status, processing options, segment name, key feedback name, number of sensitive segments, and the key feedback buffer. The structure facilitates communication between the application and the IMS PASFL database for segment search and access.

**Business Context**: IMS hierarchical database (DL/I) access for the PASFL database

## Paragraphs/Procedures

### ~~PASFLPCB~~ (Dead Code)
*Record layout 'PASFLPCB' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| PASFLPCB | record_layout | 1 | Record layout 'PASFLPCB' is never used by any program |

## Open Questions

- ? Which specific IMS application programs include and use this PASFLPCB copybook?
  - Context: The copybook file does not reference any calling programs or PSBs
- ? What is the exact PSB (Program Specification Block) associated with this PCB?
  - Context: File contains only the PCB definition, no PSB name or full PSB context
