# CCPAUERY

**File**: `cpy/CCPAUERY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-27 14:44:14.139508

## Purpose

This copybook defines the structure of an error log record, used for logging errors, warnings, and critical events within an application. It includes fields for date, time, application name, program name, location, error level, subsystem, error codes, message, and an event key.

## Paragraphs/Procedures

### ~~ERROR-LOG-RECORD~~ (Dead Code)
*Record layout 'ERROR-LOG-RECORD' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| ERROR-LOG-RECORD | record_layout | 1 | Record layout 'ERROR-LOG-RECORD' is never used by any program |

## Open Questions

- ? How is the ERROR-LOG-RECORD populated and written to a log?
  - Context: The copybook only defines the structure, not the usage.
