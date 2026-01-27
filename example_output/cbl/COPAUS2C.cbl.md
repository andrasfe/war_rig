# COPAUS2C

**File**: `cbl/COPAUS2C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-27 02:40:29.241777

## Purpose

This CICS COBOL program receives authorization details and fraud marking instructions via COMMAREA, formats timestamps, and inserts a new record into the CARDDEMO.AUTHFRDS DB2 table to mark the authorization as fraud. If the record already exists (SQLCODE -803), it performs an UPDATE to set the fraud flag and report date instead. Upon completion, it sets the status in the output COMMAREA and returns to CICS.

**Business Context**: CardDemo application - Authorization Module for marking fraudulent authorization messages.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DFHCOMMAREA | IOType.CICS_COMMAREA | Contains account ID, customer ID, fraud authorization record details (PA- fields from CIPAUDTY copybook), and fraud status record including action flag ('F' for report fraud). |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | DB2 table storing authorization fraud details; new records inserted or existing updated with fraud flag, report date, and related transaction data. |
| DFHCOMMAREA | IOType.CICS_COMMAREA | Updated with fraud status record indicating success/failure and message. |

## Business Rules

- **BR001**: Handle duplicate key on INSERT by updating the existing record instead of failing.

## Paragraphs/Procedures

### MAIN-PARA
This is the main orchestration paragraph serving as the program's entry point and controlling the entire fraud marking process. It consumes inputs from the DFHCOMMAREA linkage section, including WS-ACCT-ID, WS-CUST-ID, WS-FRD-ACTION, and numerous PA- fields from the CIPAUDTY copybook structure within WS-FRAUD-AUTH-RECORD. It first obtains current time via CICS ASKTIME (91) and formats the date via FORMATTIME into WS-CUR-DATE, moving it to PA-FRAUD-RPT-DATE (101). It then parses PA-AUTH-ORIG-DATE into date components (103-105) and computes/formats WS-AUTH-TIME from PA-AUTH-TIME-9C (107-111). Next, it moves all relevant input fields to SQL host variables (e.g., CARD-NUM from PA-CARD-NUM at 113, AUTH-FRAUD from WS-FRD-ACTION at 137, up to CUST-ID at 139). The core business logic performs SQL INSERT into AUTHFRDS with formatted timestamp and CURRENT DATE (141-198), deciding based on SQLCODE: success if 0 (sets WS-FRD-UPDT-SUCCESS and message 200-201), duplicate handling via PERFORM FRAUD-UPDATE if -803 (204), or failure with error message construction using WS-SQLCODE/STATE (206-214). Outputs include the updated DB2 table and modified WS-FRAUD-STATUS-RECORD in COMMAREA. Error handling covers SQL errors post-INSERT but uses NOHANDLE for CICS commands. It calls FRAUD-UPDATE conditionally for duplicates. Finally, it executes CICS RETURN (218) to end the transaction.

### FRAUD-UPDATE
This paragraph performs the SQL UPDATE for existing AUTHFRDS records when INSERT detects a duplicate key (SQLCODE -803). It consumes host variables populated in MAIN-PARA, specifically CARD-NUM, AUTH-TS (formatted), AUTH-FRAUD, using them in the WHERE clause and SET for AUTH_FRAUD and FRAUD_RPT_DATE = CURRENT DATE (222-228). The primary purpose is to overwrite the fraud status on matching card number and auth timestamp without inserting a new record. It produces updates to the DB2 table and sets WS-FRD-STATUS-RECORD accordingly. Business logic checks SQLCODE post-UPDATE: if 0, sets WS-FRD-UPDT-SUCCESS and 'UPDT SUCCESS' message (231-232); else sets WS-FRD-UPDT-FAILED and builds error string with SQLCODE/SQLSTATE (234-241). No additional validations or conditions are checked beyond SQL success. Error handling is SQLCODE-based, mirroring MAIN-PARA. It makes no calls to other paragraphs or programs. Control returns implicitly to MAIN-PARA after PERFORM completion.

## Open Questions

- ? Exact field definitions in CIPAUDTY copybook?
  - Context: Fields like PA-AUTH-ORIG-DATE, PA-CARD-NUM used extensively but structure not inline.
- ? Role of IMS in header mention?
  - Context: Header states 'CICS COBOL IMS DB2 Program' but no IMS DL/I calls visible; only DB2 SQL.
