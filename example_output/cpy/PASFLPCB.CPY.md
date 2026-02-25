# PASFLPCB

**File**: `cpy/PASFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-25 15:31:47.425731

## Purpose

This COBOL copybook defines the PASFLPCB 01-level record layout, which represents the Program Communication Block (PCB) for the IMS DL/I database named PASFL. It includes fields for database name, segment level, PCB status, processing options, segment name, key feedback name, number of sensitive segments, and the key feedback buffer area. This structure is used by IMS-enabled programs to interface with DL/I calls for database navigation and access.

**Business Context**: Facilitates IMS hierarchical database (DL/I) access and control for the PASFL database in mainframe batch or online applications.

## Paragraphs/Procedures

### ~~PASFLPCB~~ (Dead Code)
*Record layout 'PASFLPCB' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| PASFLPCB | record_layout | 1 | Record layout 'PASFLPCB' is never used by any program |

## Open Questions

- ? Is PASFL-SEG-LEVEL a standard or custom field in the IMS PCB layout?
  - Context: Standard IMS DB PCB layouts typically position PCB-STATUS immediately after DBDNAME, without a preceding 2-byte segment level field.
