---
name: copaus2c
description: "This CICS COBOL program marks an authorization message as fraud by inserting a record into the CARDDEMO.AUTHFRDS DB2 table using data from the commarea. If the insert fails due to a duplicate key error (SQLCODE -803), it performs an update on the existing record to set the fraud flag and report date. Upon completion, it sets status messages in the commarea and returns to CICS."
---

# COPAUS2C

**Type:** COBOL (ONLINE_CICS)
**Context:** CardDemo Authorization Module: Handles fraud reporting for authorization transactions by logging or updating fraud indicators in the AUTHFRDS table.

## Purpose

This CICS COBOL program marks an authorization message as fraud by inserting a record into the CARDDEMO.AUTHFRDS DB2 table using data from the commarea. If the insert fails due to a duplicate key error (SQLCODE -803), it performs an update on the existing record to set the fraud flag and report date. Upon completion, it sets status messages in the commarea and returns to CICS.

## Business Rules

- **BR001**: If fraud action is to report fraud (WS-FRD-ACTION='F'), insert new record into AUTHFRDS; if duplicate (-803), update existing record to set AUTH_FRAUD='F' and current FRAUD_RPT_DATE.
- **BR002**: On SQL error not zero or -803 (insert) or not zero (update), set failure flag and build error message with SQLCODE/SQLSTATE.

## Inputs

- **DFHCOMMAREA** (CICS_COMMAREA): Contains WS-ACCT-ID, WS-CUST-ID, WS-FRAUD-AUTH-RECORD (from CIPAUDTY copybook with PA- fields like PA-AUTH-ORIG-DATE, PA-CARD-NUM), and WS-FRAUD-STATUS-RECORD (WS-FRD-ACTION, etc.) providing authorization details and fraud action flag.
- **SQLCA** (OTHER): SQL communication area for DB2 error codes and states (SQLCODE, SQLSTATE).

## Outputs

- **DFHCOMMAREA** (CICS_COMMAREA): Updated with WS-FRD-UPDT-SUCCESS/FAILED flags and WS-FRD-ACT-MSG status message before CICS RETURN.
- **CARDDEMO.AUTHFRDS** (DB2_TABLE): Fraud authorization records inserted or updated with fields like CARD_NUM, AUTH_TS, AUTH_FRAUD='F', FRAUD_RPT_DATE.

## Copybooks Used

- **CIPAUDTY**: Defines WS-FRAUD-AUTH-RECORD structure with PA- fields (e.g., PA-CARD-NUM, PA-AUTH-ORIG-DATE, PA-AUTH-TYPE) for authorization data from commarea.
- **AUTHFRDS**: SQL INCLUDE defining host variables (e.g., CARD-NUM, AUTH-TS) matching CARDDEMO.AUTHFRDS table columns.
- **DFHBMSCA**: CICS BMS symbolic map copybook, unused in visible code.
- **SQLCA**: SQL communication area for error handling (SQLCODE, SQLSTATE).

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAUS2C
- Understand business rules implemented in COPAUS2C
- Identify inputs/outputs for COPAUS2C
- Maintain or modify COPAUS2C

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.