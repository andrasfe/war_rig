# COPAUS2C

**File**: `cbl/COPAUS2C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-25 18:31:17.060260

## Purpose

This CICS COBOL program receives authorization message data including card details and fraud action via COMMAREA, formats timestamps, and attempts to insert a fraud record into the CARDDEMO.AUTHFRDS DB2 table. If a duplicate key error (SQLCODE -803) occurs, it updates the existing record with fraud status and report date instead. It updates the COMMAREA with success/failure status and message before returning.

**Business Context**: CardDemo application authorization module for marking authorization messages as fraud.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DFHCOMMAREA | IOType.CICS_COMMAREA | Contains WS-ACCT-ID, WS-CUST-ID, WS-FRAUD-AUTH-RECORD (PA- fields from CIPAUDTY copybook defining authorization details like PA-CARD-NUM, PA-AUTH-ORIG-DATE), and WS-FRAUD-STATUS-RECORD (action 'F' or 'R') |
| CICS_ABSTIME | IOType.CICS_COMMAREA | Current absolute time used to format current date for FRAUD_RPT_DATE |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| DFHCOMMAREA | IOType.CICS_COMMAREA | Updated with WS-FRD-UPDATE-STATUS ('S' or 'F') and WS-FRD-ACT-MSG (success/error message) |

## Business Rules

- **BR001**: If INSERT succeeds (SQLCODE = 0), set update status to success and message to 'ADD SUCCESS'
- **BR002**: If INSERT fails with duplicate key (SQLCODE = -803), perform UPDATE on existing record
- **BR003**: If UPDATE succeeds (SQLCODE = 0), set update status to success and message to 'UPDT SUCCESS'
- **BR004**: On any other SQL error, set update status to failed and construct error message with SQLCODE and SQLSTATE

## Paragraphs/Procedures

### MAIN-PARA
Main logic: get current time, format timestamps, move data to host vars, perform INSERT, handle errors including UPDATE, set status, RETURN

### FRAUD-UPDATE
UPDATE existing AUTHFRDS record with AUTH_FRAUD and FRAUD_RPT_DATE on duplicate key

## Open Questions

- ? Exact structure and fields of CIPAUDTY copybook
  - Context: PA- fields (e.g., PA-AUTH-ORIG-DATE, PA-AUTH-TIME-9C) are referenced but not defined in source
- ? Exact host variables defined by SQL INCLUDE AUTHFRDS
  - Context: Fields like CARD-NUM, AUTH-TS used post-INCLUDE but definitions not in source
- ? Purpose of DFHBMSCA copybook inclusion
  - Context: Included but no BMS MAP operations or references in code
- ? Why header mentions IMS DB2 but no IMS DL/I calls visible
  - Context: Comment line 4 says 'CICS COBOL IMS DB2 Program' but only CICS/DB2 used
