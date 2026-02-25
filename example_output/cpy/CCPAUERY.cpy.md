# CCPAUERY

**File**: `cpy/CCPAUERY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-25 15:31:40.484893

## Purpose

This copybook defines the ERROR-LOG-RECORD structure for logging errors in pending authorization processes. It includes fields for timestamp (ERR-DATE, ERR-TIME), identifiers (ERR-APPLICATION, ERR-PROGRAM, ERR-LOCATION), severity (ERR-LEVEL with 88-level conditions), subsystem (ERR-SUBSYSTEM with 88-level conditions), error codes (ERR-CODE-1, ERR-CODE-2), message (ERR-MESSAGE), and event key (ERR-EVENT-KEY). The structure standardizes error logging across mainframe applications.

**Business Context**: Facilitates standardized error logging for pending authorization errors in COBOL mainframe applications, as indicated by the header documentation.

## Business Rules

- **BR001**: Defines valid error severity levels using condition names on ERR-LEVEL field.
- **BR002**: Defines valid error subsystems using condition names on ERR-SUBSYSTEM field.

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
