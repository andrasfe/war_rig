# CCPAUERY

**File**: `cpy/CCPAUERY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 15:17:07.916490

## Purpose

This COBOL copybook defines the ERROR-LOG-RECORD level 01 data structure for logging pending authorization errors. It includes fields for error date (PIC X(06)), time (PIC X(06)), application (PIC X(08)), program (PIC X(08)), location (PIC X(04)), level (PIC X(01)) with 88 levels for LOG/I/WARNING/CRITICAL, subsystem (PIC X(01)) with 88 levels for APP/CICS/IMS/DB2/MQ/FILE, two error codes (PIC X(09) each), message (PIC X(50)), and event key (PIC X(20)). The structure standardizes error logging across mainframe applications.

**Business Context**: Supports standardized error logging for pending authorization processes in Amazon's mainframe environment, capturing diagnostics for applications, subsystems, and events.
