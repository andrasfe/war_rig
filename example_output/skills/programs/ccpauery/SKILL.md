---
name: ccpauery
description: "This COBOL copybook defines the ERROR-LOG-RECORD structure for capturing details of errors in pending authorization processes. It includes fields for timestamp (ERR-DATE, ERR-TIME), application and program identifiers (ERR-APPLICATION, ERR-PROGRAM), error location (ERR-LOCATION), severity level (ERR-LEVEL with 88-level condition names for LOG, INFO, WARNING, CRITICAL), subsystem type (ERR-SUBSYSTEM with 88-levels for APP, CICS, IMS, DB2, MQ, FILE), error codes (ERR-CODE-1, ERR-CODE-2), message text (ERR-MESSAGE), and an event key (ERR-EVENT-KEY). The copybook is licensed under Apache License 2.0 and copyrighted by Amazon.com."
---

# CCPAUERY

**Type:** COPYBOOK (UTILITY)
**Context:** Supports error logging in Amazon's mainframe applications handling pending authorizations, across subsystems like CICS, IMS, DB2, MQ, and files.

## Purpose

This COBOL copybook defines the ERROR-LOG-RECORD structure for capturing details of errors in pending authorization processes. It includes fields for timestamp (ERR-DATE, ERR-TIME), application and program identifiers (ERR-APPLICATION, ERR-PROGRAM), error location (ERR-LOCATION), severity level (ERR-LEVEL with 88-level condition names for LOG, INFO, WARNING, CRITICAL), subsystem type (ERR-SUBSYSTEM with 88-levels for APP, CICS, IMS, DB2, MQ, FILE), error codes (ERR-CODE-1, ERR-CODE-2), message text (ERR-MESSAGE), and an event key (ERR-EVENT-KEY). The copybook is licensed under Apache License 2.0 and copyrighted by Amazon.com.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of CCPAUERY
- Maintain or modify CCPAUERY

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.