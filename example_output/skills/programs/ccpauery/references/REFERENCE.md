# CCPAUERY - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** CCPAUERY
- **File Name:** cpy/CCPAUERY.cpy
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:48:19.986718

## Purpose

**Summary:** This COBOL copybook defines the ERROR-LOG-RECORD structure for capturing details of errors in pending authorization processes. It includes fields for timestamp (ERR-DATE, ERR-TIME), application and program identifiers (ERR-APPLICATION, ERR-PROGRAM), error location (ERR-LOCATION), severity level (ERR-LEVEL with 88-level condition names for LOG, INFO, WARNING, CRITICAL), subsystem type (ERR-SUBSYSTEM with 88-levels for APP, CICS, IMS, DB2, MQ, FILE), error codes (ERR-CODE-1, ERR-CODE-2), message text (ERR-MESSAGE), and an event key (ERR-EVENT-KEY). The copybook is licensed under Apache License 2.0 and copyrighted by Amazon.com.

**Business Context:** Supports error logging in Amazon's mainframe applications handling pending authorizations, across subsystems like CICS, IMS, DB2, MQ, and files.
**Program Type:** UTILITY

## Paragraphs

### CCPAUERY

[Citadel] Paragraph identified by static analysis

### ERROR-LOG-RECORD

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Dead Code

- **ERROR-LOG-RECORD** (record_layout): Record layout 'ERROR-LOG-RECORD' is never used by any program
