# CCPAUERY

**File**: `cpy/CCPAUERY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-03 16:50:19.696039

## Purpose

This copybook defines the structure of the ERROR-LOG-RECORD, which is used for logging errors and informational messages within an application. It includes fields for date, time, application name, program name, location, severity level, subsystem, error codes, message text, and an event key.

## Paragraphs/Procedures

### ~~ERROR-LOG-RECORD~~ (Dead Code)
*Record layout 'ERROR-LOG-RECORD' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| ERROR-LOG-RECORD | record_layout | 1 | Record layout 'ERROR-LOG-RECORD' is never used by any program |

## Open Questions

- ? How is the ERR-LOCATION field populated and what does it represent?
  - Context: The purpose of the ERR-LOCATION field is unclear from the copybook definition.
