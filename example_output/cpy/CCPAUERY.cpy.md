# CCPAUERY

**File**: `cpy/CCPAUERY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 14:25:11.291718

## Purpose

This COBOL copybook defines the ERROR-LOG-RECORD level-01 data structure for capturing details of errors in pending authorization processes. It includes fields for timestamp (ERR-DATE, ERR-TIME), application identifiers (ERR-APPLICATION, ERR-PROGRAM), location (ERR-LOCATION), severity level (ERR-LEVEL with 88 conditions), subsystem (ERR-SUBSYSTEM with 88 conditions), error codes (ERR-CODE-1, ERR-CODE-2), message (ERR-MESSAGE), and event key (ERR-EVENT-KEY). The structure supports standardized error logging across mainframe applications.

**Business Context**: Error logging for pending authorization workflows in Amazon mainframe systems, enabling categorization by severity and subsystem for auditing and diagnostics.

## Business Rules

- **BR001**: Valid values for error severity level (ERR-LEVEL) are restricted to Log ('L'), Info ('I'), Warning ('W'), or Critical ('C')
- **BR002**: Valid values for error subsystem (ERR-SUBSYSTEM) are restricted to Application ('A'), CICS ('C'), IMS ('I'), DB2 ('D'), MQ ('M'), or File ('F')

## Open Questions

- ? In which specific programs and sections (e.g., WORKING-STORAGE, LINKAGE) is this CCPAUERY copybook included?
  - Context: Copybook analysis does not reveal usage context; only the structure definition is present
