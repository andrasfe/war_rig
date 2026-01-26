# COPAUS2C

**File**: `cbl/COPAUS2C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 15:11:40.757393

## Purpose

This CICS COBOL program marks credit card authorization messages as fraud (or updates status) by inserting a full record into the CARDDEMO.AUTHFRDS DB2 table or updating an existing one if duplicate key error occurs. It receives authorization details, account/customer IDs, and fraud action via COMMAREA, formats timestamps using CICS time services, populates SQL host variables, and handles success/failure by updating COMMAREA status fields before returning to CICS.

**Business Context**: CardDemo application authorization module for logging fraud reports on authorization transactions in a payment processing system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DFHCOMMAREA | IOType.CICS_COMMAREA | Contains account ID, customer ID, fraud authorization record with auth details (PA- fields), and fraud status record including action ('F' report fraud, 'R' remove) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Fraud authorization records storing full auth transaction details, fraud flag, report date, account ID, and customer ID |
| DFHCOMMAREA | IOType.CICS_COMMAREA | Updated with fraud status record fields WS-FRD-UPDT-STATUS and WS-FRD-ACT-MSG reflecting insert/update success or failure |

## Business Rules

- **BR001**: On successful INSERT (SQLCODE=0), set update status to success and message 'ADD SUCCESS'
- **BR002**: On INSERT duplicate key error (SQLCODE=-803), perform UPDATE to set fraud flag and report date on existing record
- **BR003**: On any other SQL error after INSERT or UPDATE, set update status to failed and build error message from SQLCODE/SQLSTATE
- **BR004**: Set AUTH_FRAUD field to value from input WS-FRD-ACTION ('F' or 'R')

## Paragraphs/Procedures

### MAIN-PARA
This is the main entry point and orchestration paragraph controlling the entire program flow for fraud marking. It consumes input from DFHCOMMAREA including WS-ACCT-ID, WS-CUST-ID, PA- authorization fields from CIPAUDTY, and WS-FRD-ACTION. It first obtains current absolute time via CICS ASKTIME into WS-ABS-TIME and formats it to MMDDYY in WS-CUR-DATE using FORMATTIME, then moves WS-CUR-DATE to PA-FRAUD-RPT-DATE in commarea. It parses PA-AUTH-ORIG-DATE into date parts of WS-AUTH-TS, computes adjusted time WS-AUTH-TIME = 999999999 - PA-AUTH-TIME-9C, and formats time parts into full AUTH-TS timestamp string. It populates all SQL host variables from AUTHFRDS include by moving input PA- fields (CARD-NUM, AUTH-TYPE, etc.), WS-FRD-ACTION to AUTH-FRAUD, WS-ACCT-ID/CUST-ID, and prepares MERCHANT-NAME with length. The core business logic performs INSERT into AUTHFRDS with CURRENT DATE for FRAUD_RPT_DATE. It evaluates SQLCODE post-INSERT: if 0, sets success status/message; if -803 duplicate, PERFORMs FRAUD-UPDATE; else sets failure and strings SQLCODE/SQLSTATE error into WS-FRD-ACT-MSG. Error handling relies on SQLCA without additional validation beyond dupkey. Finally, EXEC CICS RETURN ends the transaction, producing updated commarea status and modified DB2 table.

### FRAUD-UPDATE
This subordinate paragraph handles idempotent update for duplicate authorization records after failed INSERT. It consumes pre-populated SQL host variables CARD-NUM, AUTH-TS, AUTH-FRAUD from MAIN-PARA. It produces an UPDATE to AUTHFRDS setting AUTH_FRAUD = :AUTH-FRAUD and FRAUD_RPT_DATE = CURRENT DATE WHERE CARD_NUM and AUTH_TS match input. The business logic ensures fraud marking overrides without duplication for same card/timestamp. Post-UPDATE, it checks SQLCODE: if 0, sets WS-FRD-UPDT-SUCCESS and 'UPDT SUCCESS' message; else sets WS-FRD-UPDT-FAILED and strings SQLCODE/SQLSTATE into WS-FRD-ACT-MSG. Error handling is via SQLCA check with no further actions or validations. No subordinate calls or loops, returns control to caller (MAIN-PARA). Outputs update the DB2 record and commarea status for return to CICS.

## Open Questions

- ? Definition and PIC clause of PA-AUTH-TIME-9C
  - Context: Used in COMPUTE WS-AUTH-TIME but not defined in source; assumed packed decimal in CIPAUDTY
- ? CICS transaction ID or entry point for this program
  - Context: Not present in source code
- ? Role of IMS in 'CICS COBOL IMS DB2 Program' header
  - Context: No DL/I or IMS calls visible; possibly environmental
- ? Purpose of MOVE WS-CUR-DATE TO PA-FRAUD-RPT-DATE
  - Context: PA-FRAUD-RPT-DATE modified but not used in SQL (uses CURRENT DATE)
