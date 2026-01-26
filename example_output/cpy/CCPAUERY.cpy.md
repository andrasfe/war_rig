# CCPAUERY

**File**: `cpy/CCPAUERY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 02:34:09.314977

## Purpose

This copybook defines the ERROR-LOG-RECORD data structure for logging pending authorization errors in mainframe applications. It specifies fields for timestamp (ERR-DATE, ERR-TIME), identifiers (ERR-APPLICATION, ERR-PROGRAM, ERR-LOCATION), severity (ERR-LEVEL with 88-levels), subsystem (ERR-SUBSYSTEM with 88-levels), error codes (ERR-CODE-1, ERR-CODE-2), message (ERR-MESSAGE), and event key (ERR-EVENT-KEY). The structure standardizes error log entries across applications.

**Business Context**: Facilitates standardized error logging for pending authorization processes in mainframe environments, supporting subsystems such as CICS, IMS, DB2, MQ, and file I/O.

## Outputs

| Name | Type | Description |
|------|------|-------------|
| ERROR-LOG-RECORD | IOType.OTHER | Data structure defining the complete error log record layout, populated by calling programs and written to error log files for auditing and diagnostics. |

## Business Rules

- **BR001**: Categorizes error severity levels using ERR-LEVEL field with valid values 'L' (LOG), 'I' (INFO), 'W' (WARNING), 'C' (CRITICAL)
- **BR002**: Identifies originating subsystem using ERR-SUBSYSTEM field with valid values 'A' (APP), 'C' (CICS), 'I' (IMS), 'D' (DB2), 'M' (MQ), 'F' (FILE)
