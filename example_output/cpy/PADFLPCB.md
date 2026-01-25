# PADFLPCB

**File**: `cpy/PADFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-25 18:37:49.796419

## Purpose

This copybook defines the 01 level record PADFLPCB, a Processing Control Block (PCB) structure for IMS DL/I database interface. It includes fields for database name (PADFL-DBDNAME), segment level (PADFL-SEG-LEVEL), PCB status (PADFL-PCB-STATUS), processing options (PADFL-PCB-PROCOPT), segment name (PADFL-SEG-NAME), key feedback name offset (PADFL-KEYFB-NAME), number of sensitive segments (PADFL-NUM-SENSEGS), and key feedback buffer (PADFL-KEYFB). A filler field is present between PROCOPT and SEG-NAME.

**Business Context**: Supports IMS hierarchical database (DL/I) access in mainframe COBOL programs by defining the standard PCB layout

## Business Rules

- **BR001**: PADFL-PCB-STATUS holds the 2-byte IMS PCB status code set by IMS after DL/I calls
- **BR002**: PADFL-PCB-PROCOPT holds the 4-byte processing options set by the application for IMS DL/I calls (e.g., GO, UPD)

## Open Questions

- ? In which COBOL section (e.g., LINKAGE_SECTION, WORKING-STORAGE_SECTION) is this copybook typically COPY'ed?
  - Context: Copybook itself does not specify inclusion context; standard IMS usage is in LINKAGE_SECTION for PCBs
- ? What is the exact mapping of FILLER field (line 22) in standard IMS PCB layout?
  - Context: Code defines it as PIC S9(05) COMP but field name is FILLER; purpose undetermined from code alone
