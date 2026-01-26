# COPAUS2C

**File**: `cbl/COPAUS2C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 17:37:29.459364

## Purpose

This CICS COBOL program receives authorization transaction data via commarea, formats timestamps, and inserts a new record into the CARDDEMO.AUTHFRDS DB2 table to mark the authorization as fraud based on the action flag. If the insert fails due to duplicate key (SQLCODE -803), it updates the existing matching record instead. It sets success or failure status and message in the commarea before returning to CICS.

**Business Context**: CardDemo application Authorization Module for marking fraud on authorization messages

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DFHCOMMAREA | IOType.CICS_COMMAREA | Linkage area containing WS-ACCT-ID, WS-CUST-ID, WS-FRAUD-AUTH-RECORD (PA- fields from CIPAUDTY), and WS-FRAUD-STATUS-RECORD (action flag, status, message) |
| CICS_ABSTIME | IOType.CICS_COMMAREA | Current absolute time retrieved via ASKTIME for date/time formatting |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Table storing fraud-marked authorization records with details like card number, auth timestamp, fraud flag, report date, acct/cust IDs |
| DFHCOMMAREA | IOType.CICS_COMMAREA | Updated with WS-FRD-UPDT-STATUS and WS-FRD-ACT-MSG reflecting insert/update success or failure |

## Business Rules

- **BR001**: Attempt INSERT into AUTHFRDS; on duplicate key SQLCODE -803, perform UPDATE instead to mark existing auth record as fraud
- **BR002**: Set AUTH_FRAUD flag from WS-FRD-ACTION ('F' for fraud report, 'R' for remove)

## Paragraphs/Procedures

### MAIN-PARA
This is the main orchestration paragraph serving as the program's entry point and controlling the entire transaction flow in this CICS program. It first retrieves the current absolute time via EXEC CICS ASKTIME into WS-ABS-TIME and formats it to MMDDYY date string in WS-CUR-DATE using FORMATTIME, then moves this to PA-FRAUD-RPT-DATE in the linkage commarea. It consumes PA-AUTH-ORIG-DATE from the CIPAUDTY copybook record to populate date parts (YY, MM, DD) in WS-AUTH-TS and computes authorization time by subtracting PA-AUTH-TIME-9C from 999999999, parsing the result into HH, MI, SS, SSS for a full timestamp in WS-AUTH-TS. The paragraph reads extensively from linkage DFHCOMMAREA fields including all PA- prefixed authorization details, WS-ACCT-ID, WS-CUST-ID, and WS-FRD-ACTION, moving them to SQL host variables such as CARD-NUM, AUTH-TYPE, TRANSACTION-AMT, MERCHANT-ID, AUTH-FRAUD, etc., and computes MERCHANT-NAME-LEN. It then performs an EXEC SQL INSERT into CARDDEMO.AUTHFRDS populating 22 fields with the prepared data, using TIMESTAMP_FORMAT on AUTH-TS and CURRENT DATE for FRAUD_RPT_DATE. Business logic checks SQLCODE post-INSERT: if 0, sets WS-FRD-UPDT-SUCCESS and 'ADD SUCCESS' message; if -803 (duplicate), PERFORMs FRAUD-UPDATE; else sets failure, captures WS-SQLCODE/WS-SQLSTATE, and STRINGs error into WS-FRD-ACT-MSG. Error handling is SQLCODE-driven with no abend, just status updates in commarea. It calls FRAUD-UPDATE conditionally for duplicate handling. Finally, EXEC CICS RETURN releases control back to CICS with updated commarea.

### FRAUD-UPDATE
This paragraph handles the duplicate key scenario from MAIN-PARA by updating an existing AUTHFRDS record instead of inserting a new one. It consumes host variables prepared in MAIN-PARA, specifically CARD-NUM, AUTH-TS (timestamp formatted), and AUTH-FRAUD (from WS-FRD-ACTION). Its primary role is to execute an EXEC SQL UPDATE on CARDDEMO.AUTHFRDS setting AUTH_FRAUD = :AUTH-FRAUD and FRAUD_RPT_DATE = CURRENT DATE WHERE CARD_NUM = :CARD-NUM AND AUTH_TS matches the formatted timestamp. No additional data reads or complex validations; it relies on the uniqueness of CARD_NUM + AUTH_TS for targeting the record. Post-UPDATE, it checks SQLCODE: if 0, sets WS-FRD-UPDT-SUCCESS and moves 'UPDT SUCCESS' to WS-FRD-ACT-MSG; else sets WS-FRD-UPDT-FAILED, captures SQLCODE/SQLSTATE to WS-SQLCODE/WS-SQLSTATE, and STRINGs 'UPDT ERROR DB2: CODE: ... STATE: ...' into WS-FRD-ACT-MSG. Error handling mirrors MAIN-PARA's SQL failure path, updating commarea status without abending. It produces database updates to fraud flag and report date on the matching record, and modifies linkage WS-FRD-STATUS-RECORD fields. No subordinate calls or loops; execution is linear and terminates implicitly after status set. This implements the business rule for idempotent fraud marking on potential duplicates.
