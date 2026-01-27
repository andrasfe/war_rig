# CCPAUERY

**File**: `cpy/CCPAUERY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 02:44:55.629482

## Purpose

This copybook defines the ERROR-LOG-RECORD data structure for logging errors related to pending authorizations. It captures key error details such as date (ERR-DATE), time (ERR-TIME), application (ERR-APPLICATION), program (ERR-PROGRAM), location (ERR-LOCATION), severity level (ERR-LEVEL with 88 condition names for LOG, INFO, WARNING, CRITICAL), subsystem (ERR-SUBSYSTEM with 88 conditions for APP, CICS, IMS, DB2, MQ, FILE), error codes (ERR-CODE-1, ERR-CODE-2), message (ERR-MESSAGE), and event key (ERR-EVENT-KEY). The structure is intended for use in error logging by mainframe applications.

**Business Context**: Supports error logging in mainframe environments for pending authorization processes, accommodating various subsystems like CICS, IMS, DB2, MQ, and file I/O.

## Open Questions

- ? In which specific programs or modules is this copybook included?
  - Context: The copybook file does not specify usage locations; it would be determined from including programs.
- ? What is the exact output destination (file, queue) for records using this structure?
  - Context: Not defined in the copybook; depends on consuming program.
