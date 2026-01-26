# COPAUS2C

**File**: `cbl/COPAUS2C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 14:21:08.902004

## Purpose

This CICS COBOL program processes fraud marking requests for authorization messages in the CardDemo application. It receives authorization details, account ID, customer ID, and fraud action via COMMAREA, formats timestamps, and inserts a new record into the CARDDEMO.AUTHFRDS DB2 table. If a duplicate key error (SQLCODE -803) occurs, it updates the existing record instead; otherwise, it sets success or failure status in the COMMAREA and returns.

**Business Context**: CardDemo Authorization Module: Marks authorization messages as fraudulent

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DFHCOMMAREA | IOType.CICS_COMMAREA | Contains WS-ACCT-ID, WS-CUST-ID, WS-FRAUD-AUTH-RECORD (authorization details like PA-AUTH-ORIG-DATE, PA-CARD-NUM, etc.), and WS-FRAUD-STATUS-RECORD (fraud action 'F' or 'R') |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| DFHCOMMAREA | IOType.CICS_COMMAREA | Updated with WS-FRD-UPDT-SUCCESS/FAILED status and WS-FRD-ACT-MSG (success/error message) |
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Fraud authorization records inserted or updated with details like CARD_NUM, AUTH_TS, AUTH_FRAUD='F', FRAUD_RPT_DATE, ACCT_ID, CUST_ID |

## Business Rules

- **BR001**: Insert new fraud authorization record; on duplicate key (SQLCODE -803), update existing record's AUTH_FRAUD and FRAUD_RPT_DATE
- **BR002**: Set success status and message on successful INSERT or UPDATE; set failure and SQL error details otherwise

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph for the entire program flow in this CICS transaction. It begins by executing CICS ASKTIME and FORMATTIME to obtain current absolute time and formatted date, storing in WS-ABS-TIME and WS-CUR-DATE, then moves WS-CUR-DATE to PA-FRAUD-RPT-DATE (line 101). It consumes input from DFHCOMMAREA linkage fields including WS-ACCT-ID, WS-CUST-ID, WS-FRD-ACTION, and WS-FRAUD-AUTH-RECORD (PA- fields like PA-AUTH-ORIG-DATE, PA-CARD-NUM). It transforms and formats the authorization timestamp by parsing PA-AUTH-ORIG-DATE into WS-AUTH-YY/MM/DD and computing inverted time from PA-AUTH-TIME-9C into WS-AUTH-HH/MI/SS/SSS, assembling WS-AUTH-TS (lines 103-111). It then moves numerous PA- fields to host variables defined by AUTHFRDS include (e.g., CARD-NUM, AUTH-TYPE, lines 113-139). The core business logic performs an SQL INSERT into CARDDEMO.AUTHFRDS with formatted data, CURRENT DATE for FRAUD_RPT_DATE, and WS-FRD-ACTION as AUTH_FRAUD (lines 141-198). It checks SQLCODE: if 0, sets success status and message; if -803 (duplicate), performs FRAUD-UPDATE paragraph; else, sets failure with SQLCODE/STATE in message (lines 199-216). No explicit file I/O errors beyond SQL, but SQL errors are captured in WS-SQLCODE/STATE. Finally, it executes CICS RETURN to send updated COMMAREA back (line 218). This paragraph calls FRAUD-UPDATE only on duplicate key to handle upserts.

### FRAUD-UPDATE
This paragraph handles the update case for existing fraud authorization records when INSERT fails with duplicate key (SQLCODE -803). It is called exclusively from MAIN-PARA at line 204. It consumes host variables populated in MAIN-PARA such as AUTH-FRAUD (from WS-FRD-ACTION), CARD-NUM, AUTH-TS. It produces an SQL UPDATE on CARDDEMO.AUTHFRDS setting AUTH_FRAUD and FRAUD_RPT_DATE = CURRENT DATE, keyed by CARD_NUM and exact AUTH_TS match (lines 223-229). The business logic checks SQLCODE post-update: if 0, sets WS-FRD-UPDT-SUCCESS and 'UPDT SUCCESS' message; else, sets failure and builds error string with WS-SQLCODE/WS-SQLSTATE (lines 230-243). Error handling mirrors MAIN-PARA by capturing SQL errors into COMMAREA message fields. It produces no further calls or outputs beyond the SQL and status updates. Control returns to caller (MAIN-PARA) after completion, leading to CICS RETURN. This implements the upsert pattern for fraud marking to avoid duplicates while allowing status changes.

## Open Questions

- ? Exact structure and field definitions of CIPAUDTY copybook
  - Context: PA- fields like PA-AUTH-ORIG-DATE, PA-CARD-NUM used extensively but definitions not in source
- ? Host variable definitions from AUTHFRDS SQL INCLUDE
  - Context: Fields like CARD-NUM, AUTH-TS referenced but not declared in visible COBOL code
- ? IMS usage
  - Context: Header mentions IMS but no IMS DL/I calls visible
