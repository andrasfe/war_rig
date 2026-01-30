# CCPAUERY

**File**: `cpy/CCPAUERY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-30 19:48:19.986718

## Purpose

This COBOL copybook defines the ERROR-LOG-RECORD structure for capturing details of errors in pending authorization processes. It includes fields for timestamp (ERR-DATE, ERR-TIME), application and program identifiers (ERR-APPLICATION, ERR-PROGRAM), error location (ERR-LOCATION), severity level (ERR-LEVEL with 88-level condition names for LOG, INFO, WARNING, CRITICAL), subsystem type (ERR-SUBSYSTEM with 88-levels for APP, CICS, IMS, DB2, MQ, FILE), error codes (ERR-CODE-1, ERR-CODE-2), message text (ERR-MESSAGE), and an event key (ERR-EVENT-KEY). The copybook is licensed under Apache License 2.0 and copyrighted by Amazon.com.

**Business Context**: Supports error logging in Amazon's mainframe applications handling pending authorizations, across subsystems like CICS, IMS, DB2, MQ, and files.

## Paragraphs/Procedures

### CCPAUERY
[Citadel] Paragraph identified by static analysis

### ~~ERROR-LOG-RECORD~~ (Dead Code)
*Record layout 'ERROR-LOG-RECORD' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| ERROR-LOG-RECORD | record_layout | 1 | Record layout 'ERROR-LOG-RECORD' is never used by any program |
