# CCPAURLY

**File**: `cpy/CCPAURLY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 02:44:36.189648

## Purpose

This COBOL copybook defines the data structure for a Pending Authorization Response record. It consists of six elementary 05-level fields: PA-RL-CARD-NUM (PIC X(16)), PA-RL-TRANSACTION-ID (PIC X(15)), PA-RL-AUTH-ID-CODE (PIC X(06)), PA-RL-AUTH-RESP-CODE (PIC X(02)), PA-RL-AUTH-RESP-REASON (PIC X(04)), and PA-RL-APPROVED-AMT (PIC +9(10).99). As a copybook, it provides reusable data definitions for use in COBOL programs handling authorization responses; no executable logic is present.

## Open Questions

- ? What is the containing 01-level group name for these 05 fields?
  - Context: The copybook begins directly with 05-level items (lines 19-24), which in COBOL are typically subordinate to an unshown 01-level record description.
- ? In which programs and sections is this copybook included?
  - Context: No usage context is provided in the copybook itself.
