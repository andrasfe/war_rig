# COPAUS2C - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** COPAUS2C
- **File Name:** cbl/COPAUS2C.cbl
- **File Type:** COBOL
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:28:37.160519

## Purpose

**Summary:** This CICS COBOL program marks an authorization message as fraud by inserting a record into the CARDDEMO.AUTHFRDS DB2 table using data from the commarea. If the insert fails due to a duplicate key error (SQLCODE -803), it performs an update on the existing record to set the fraud flag and report date. Upon completion, it sets status messages in the commarea and returns to CICS.

**Business Context:** CardDemo Authorization Module: Handles fraud reporting for authorization transactions by logging or updating fraud indicators in the AUTHFRDS table.
**Program Type:** ONLINE_CICS

## Inputs

### DFHCOMMAREA

- **Type:** CICS_COMMAREA
- **Description:** Contains WS-ACCT-ID, WS-CUST-ID, WS-FRAUD-AUTH-RECORD (from CIPAUDTY copybook with PA- fields like PA-AUTH-ORIG-DATE, PA-CARD-NUM), and WS-FRAUD-STATUS-RECORD (WS-FRD-ACTION, etc.) providing authorization details and fraud action flag.
- **Copybook:** CIPAUDTY

### SQLCA

- **Type:** OTHER
- **Description:** SQL communication area for DB2 error codes and states (SQLCODE, SQLSTATE).

## Outputs

### DFHCOMMAREA

- **Type:** CICS_COMMAREA
- **Description:** Updated with WS-FRD-UPDT-SUCCESS/FAILED flags and WS-FRD-ACT-MSG status message before CICS RETURN.

### CARDDEMO.AUTHFRDS

- **Type:** DB2_TABLE
- **Description:** Fraud authorization records inserted or updated with fields like CARD_NUM, AUTH_TS, AUTH_FRAUD='F', FRAUD_RPT_DATE.
- **Copybook:** AUTHFRDS

## Business Rules

### BR001

**Description:** If fraud action is to report fraud (WS-FRD-ACTION='F'), insert new record into AUTHFRDS; if duplicate (-803), update existing record to set AUTH_FRAUD='F' and current FRAUD_RPT_DATE.

**Logic:** MOVE WS-FRD-ACTION TO AUTH-FRAUD then INSERT; on SQLCODE=-803 PERFORM FRAUD-UPDATE which does targeted UPDATE on CARD_NUM and AUTH_TS.

**Conditions:**
- `IF SQLCODE = ZERO`
- `IF SQLCODE = -803`

### BR002

**Description:** On SQL error not zero or -803 (insert) or not zero (update), set failure flag and build error message with SQLCODE/SQLSTATE.

**Logic:** SET WS-FRD-UPDT-FAILED TRUE and STRING error into WS-FRD-ACT-MSG.

**Conditions:**
- `IF SQLCODE NOT = ZERO`
- `IF SQLCODE NOT = -803`

## Paragraphs

### COPAUS2C

This is the program entry point defined by PROGRAM-ID, serving as the CICS transaction entry for the fraud marking function. It has no explicit code body visible, implying direct flow to MAIN-PARA as the primary procedure division start. No inputs are consumed directly here; control passes to MAIN-PARA which reads from DFHCOMMAREA linkage. No outputs produced directly; defers to MAIN-PARA for DB2 operations and commarea updates. No business logic or decisions implemented in this single line. No error handling performed. It effectively calls or transfers to MAIN-PARA implicitly in standard COBOL CICS structure, and static analysis notes includes like CIPAUDTY, SQLCA, AUTHFRDS which are loaded at compile time.

**Calls:** MAIN-PARA

### MAIN-PARA

This is the main orchestration paragraph controlling the entire program flow for marking authorization as fraud. It begins by consuming CICS ABSTIME via ASKTIME and formatting current date into WS-CUR-DATE using FORMATTIME, then moves it to PA-FRAUD-RPT-DATE (91-101). It reads PA- fields from WS-FRAUD-AUTH-RECORD in linkage (e.g., PA-AUTH-ORIG-DATE, PA-CARD-NUM) and transforms date/time: substrings to WS-AUTH-YY/MM/DD and computes WS-AUTH-TIME as 999999999 minus PA-AUTH-TIME-9C, parsing to WS-AUTH-TS (103-111). It then moves ~25 fields from PA- linkage to SQL host variables (e.g., CARD-NUM, AUTH-TS, AUTH-FRAUD from WS-FRD-ACTION) (113-139). The core business logic is EXEC SQL INSERT into CARDDEMO.AUTHFRDS with fraud details and CURRENT DATE for FRAUD_RPT_DATE (141-198). It checks SQLCODE: if 0, sets success and 'ADD SUCCESS' message; if -803 (duplicate), PERFORM FRAUD-UPDATE; else sets failure, builds error string with SQLCODE/SQLSTATE into WS-FRD-ACT-MSG (199-216). Error handling uses NOHANDLE on CICS calls and SQLCODE checks with abend avoidance via status messages. Finally, EXEC CICS RETURN with updated commarea (218). No loops; linear flow with 4 decision points on SQLCODE.

**Calls:** FRAUD-UPDATE

### FRAUD-UPDATE

This paragraph handles the fallback update for existing fraud records when INSERT detects duplicate key (SQLCODE -803). It consumes host variables set in MAIN-PARA: AUTH-FRAUD ('F' from WS-FRD-ACTION), CARD-NUM, AUTH-TS for WHERE clause. It produces an UPDATE to CARDDEMO.AUTHFRDS setting AUTH_FRAUD = :AUTH-FRAUD and FRAUD_RPT_DATE = CURRENT DATE, matching on CARD_NUM and AUTH_TS (222-229). Business logic targets precise record update without affecting others. It checks SQLCODE post-update: if 0, sets WS-FRD-UPDT-SUCCESS and 'UPDT SUCCESS' in WS-FRD-ACT-MSG; else sets failure and builds error string with SQLCODE/SQLSTATE (230-243). Error handling mirrors MAIN-PARA: status flags and messages, no abend. Called only from MAIN-PARA on duplicate error. Returns control to caller after update attempt.

## Data Flow

### Reads From

- **DFHCOMMAREA:** WS-FRD-ACTION, WS-ACCT-ID, WS-CUST-ID, PA-AUTH-ORIG-DATE, PA-CARD-NUM, PA-AUTH-TYPE

### Writes To

- **CARDDEMO.AUTHFRDS:** AUTH_FRAUD, FRAUD_RPT_DATE, CARD_NUM, AUTH_TS
- **DFHCOMMAREA:** WS-FRD-ACT-MSG, WS-FRD-UPDT-SUCCESS, WS-FRD-UPDT-FAILED

### Transforms

- `WS-ABS-TIME` -> `WS-CUR-DATE`: CICS FORMATTIME converts ABSTIME to MMDDYY format in WS-CUR-DATE, then moved to PA-FRAUD-RPT-DATE.
- `PA-AUTH-TIME-9C` -> `WS-AUTH-TIME`: COMPUTE WS-AUTH-TIME = 999999999 - PA-AUTH-TIME-9C, then parse to WS-AUTH-TS components (YY-MM-DD HH:MI:SSSSS).
- `PA-AUTH-ORIG-DATE` -> `WS-AUTH-YY, WS-AUTH-MM, WS-AUTH-DD`: Substring moves: (1:2) to YY, (3:2) MM, (5:2) DD.

## Error Handling

- **SQLCODE NOT = ZERO after INSERT:** If SQLCODE = -803, PERFORM FRAUD-UPDATE; else SET WS-FRD-UPDT-FAILED TRUE and STRING 'SYSTEM ERROR DB2: CODE:nnn, STATE: nnn' into WS-FRD-ACT-MSG.
- **SQLCODE NOT = ZERO after UPDATE:** SET WS-FRD-UPDT-FAILED TRUE and STRING 'UPDT ERROR DB2: CODE:nnn, STATE: nnn' into WS-FRD-ACT-MSG.
- **CICS ASKTIME/FORMATTIME errors:** NOHANDLE suppresses errors, program continues.

## SQL Operations

- **Unknown** on CARDDEMO.AUTHFRDS
- **Unknown** on CARDDEMO.AUTHFRDS

## CICS Operations

- ASKTIME
- FORMATTIME
- RETURN

## Flow Diagram

```mermaid
flowchart TD
    %% Title: COPAUS2C.cbl
    FRAUD_UPDATE["FRAUD-UPDATE"]
    CARDDEMO_AUTHFRDS__ext[("CARDDEMO.AUTHFRDS")]
    MAIN_PARA["MAIN-PARA"]
    FRAUD_UPDATE -.->|updates| CARDDEMO_AUTHFRDS__ext
    MAIN_PARA --> FRAUD_UPDATE
    MAIN_PARA -.->|writes| CARDDEMO_AUTHFRDS__ext
```