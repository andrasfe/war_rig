# COPAUS2C

**File**: `cbl/COPAUS2C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 02:30:15.483322

## Purpose

This CICS COBOL program processes authorization messages to mark them as fraud by inserting a record into the CARDDEMO.AUTHFRDS DB2 table using data from the input COMMAREA. If the INSERT fails due to a duplicate key (SQLCODE -803), it performs an UPDATE on the matching record to set the fraud flag and report date. It formats timestamps and dates from input fields, populates host variables, handles SQL errors by updating status in the COMMAREA, and returns to CICS.

**Business Context**: CardDemo application - Authorization Module for marking fraudulent authorization messages.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DFHCOMMAREA | IOType.CICS_COMMAREA | Contains WS-ACCT-ID, WS-CUST-ID, WS-FRAUD-AUTH-RECORD (authorization details like PA-CARD-NUM, PA-AUTH-ORIG-DATE), and WS-FRAUD-STATUS-RECORD (action like 'F' for fraud, status, message) |

## Business Rules

- **BR001**: On successful INSERT (SQLCODE=0), set update status to success and message to 'ADD SUCCESS'
- **BR002**: On INSERT duplicate key error, perform UPDATE instead of failing
- **BR003**: On any other SQL error after INSERT or UPDATE, set failed status and construct error message with SQLCODE and SQLSTATE

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph for the entire program flow in this CICS transaction. It first consumes current system time via CICS ASKTIME into WS-ABS-TIME and formats the date into WS-CUR-DATE using FORMATTIME, then moves it to PA-FRAUD-RPT-DATE (though this field is not used in SQL). It reads authorization data from LINKAGE DFHCOMMAREA's WS-FRAUD-AUTH-RECORD (PA- fields) and WS-FRAUD-STATUS-RECORD, transforming dates/times: parsing PA-AUTH-ORIG-DATE into WS-AUTH-TS components, computing relative WS-AUTH-TIME from PA-AUTH-TIME-9C, and moving all fields (card num, auth type, merchant details, etc.) to SQL host variables, plus fraud action to AUTH-FRAUD and acct/cust IDs. It produces an SQL INSERT into CARDDEMO.AUTHFRDS with all details, using TIMESTAMP_FORMAT on AUTH-TS and CURRENT DATE for FRAUD_RPT_DATE. Business logic checks SQLCODE post-INSERT: success sets WS-FRD-UPDT-SUCCESS and 'ADD SUCCESS' message; -803 triggers PERFORM FRAUD-UPDATE; other errors set failed status, move SQLCODE/STATE to WS-, and STRING into WS-FRD-ACT-MSG. No file I/O or other validations beyond SQL. It calls FRAUD-UPDATE conditionally for duplicates. Finally, EXEC CICS RETURN passes updated status back via COMMAREA. Error handling is SQL-centric with no abends.

### FRAUD-UPDATE
This paragraph serves as error recovery for duplicate key violations during INSERT, updating the existing fraud record. It consumes host variables populated in MAIN-PARA (CARD-NUM, AUTH-TS for WHERE, AUTH-FRAUD and CURRENT DATE for SET). It produces an SQL UPDATE on CARDDEMO.AUTHFRDS to set AUTH_FRAUD and FRAUD_RPT_DATE matching exact CARD_NUM and timestamp. Business logic post-UPDATE checks SQLCODE: if 0, sets WS-FRD-UPDT-SUCCESS and 'UPDT SUCCESS'; else sets failed and STRINGs SQLCODE/STATE into WS-FRD-ACT-MSG. No conditions or validations beyond SQLCODE; assumes matching record exists from prior -803. No error branching or abends; updates LINKAGE status for RETURN. Called exclusively by MAIN-PARA on SQLCODE=-803. No subordinate calls.

## Open Questions

- ? Exact field definitions in CIPAUDTY copybook (e.g., PA-AUTH-TIME-9C type)?
  - Context: Fields like PA-AUTH-TIME-9C referenced but not defined in source
- ? Purpose of MOVE WS-CUR-DATE TO PA-FRAUD-RPT-DATE at line 101?
  - Context: Field overwritten in INSERT with CURRENT DATE, not used
- ? Why header mentions IMS but no IMS commands present?
  - Context: Header: 'CICS COBOL IMS DB2 Program' but only CICS/DB2
