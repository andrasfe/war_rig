# COPAUS1C - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** COPAUS1C
- **File Name:** cbl/COPAUS1C.cbl
- **File Type:** COBOL
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:50:53.258245

## Purpose

**Summary:** This program provides a detailed view of a specific pending authorization message within the CardDemo application. It allows users to view transaction details, navigate to the next authorization record, and toggle the fraud status of a transaction.

**Business Context:** CardDemo Authorization Module - enables administrators or investigators to review flagged or pending credit card authorizations for potential fraud.
**Program Type:** ONLINE_CICS

## Inputs

### DFHCOMMAREA

- **Type:** CICS_COMMAREA
- **Description:** Contains session data including account ID and the selected authorization key from the summary screen.
- **Copybook:** COCOM01Y

### PAUTSUM0

- **Type:** IMS_SEGMENT
- **Description:** Pending Authorization Summary root segment, accessed by Account ID.
- **Copybook:** CIPAUSMY

### PAUTDTL1

- **Type:** IMS_SEGMENT
- **Description:** Pending Authorization Details child segment, containing specific transaction data.
- **Copybook:** CIPAUDTY

## Outputs

### COPAU1A

- **Type:** CICS_MAP
- **Description:** The Detail View screen displaying authorization details like card number, amount, merchant info, and fraud status.
- **Copybook:** COPAU01

### PAUTDTL1

- **Type:** IMS_SEGMENT
- **Description:** Updated detail segment when fraud status is toggled.
- **Copybook:** CIPAUDTY

## Business Rules

### BR001

**Description:** Fraud Status Toggle

**Logic:** If the record is already marked as fraud, it is toggled to removed; otherwise, it is marked as confirmed fraud.

**Conditions:**
- `IF PA-FRAUD-CONFIRMED`
- `SET PA-FRAUD-REMOVED TO TRUE`
- `SET PA-FRAUD-CONFIRMED TO TRUE`

### BR002

**Description:** Authorization Response Mapping

**Logic:** Maps response codes to 'A' (Approved/Green) or 'D' (Declined/Red) for display.

**Conditions:**
- `IF PA-AUTH-RESP-CODE = '00'`

## Paragraphs

### MAIN-PARA

This is the primary orchestration paragraph for the CICS program. It initializes program flags and checks the EIBCALEN to determine if the program was started fresh or re-entered. If the COMMAREA is empty, it redirects to the summary screen. If valid data exists, it evaluates the user's input (EIBAID) to determine whether to process the Enter key (view details), PF3 (return), PF5 (mark fraud), or PF8 (next record). It ensures the CICS session is maintained by returning with the transaction ID and COMMAREA.

**Calls:** RETURN-TO-PREV-SCREEN, PROCESS-ENTER-KEY, SEND-AUTHVIEW-SCREEN, RECEIVE-AUTHVIEW-SCREEN, MARK-AUTH-FRAUD, PROCESS-PF8-KEY

### PROCESS-ENTER-KEY

This paragraph handles the logic when the Enter key is pressed or when the program is first entered with a selection. It clears the output map and validates that the Account ID and Authorization Key are present in the COMMAREA. If valid, it calls READ-AUTH-RECORD to fetch data from the IMS database. It manages the IMS PSB state by checking if it was scheduled and then taking a syncpoint to release resources. Finally, it calls POPULATE-AUTH-DETAILS to prepare the data for the screen.

**Calls:** READ-AUTH-RECORD, TAKE-SYNCPOINT, POPULATE-AUTH-DETAILS

### MARK-AUTH-FRAUD

This paragraph implements the business logic for toggling the fraud status of a transaction. It retrieves the current record from the database and checks the existing fraud flag. It then flips the flag (Confirmed to Removed or vice versa) and prepares the WS-FRAUD-DATA structure. It performs a CICS LINK to COPAUS2C to process the fraud update logic. If the link is successful and the update succeeds, it calls UPDATE-AUTH-DETAILS to commit the change to the IMS database; otherwise, it performs a rollback.

**Calls:** READ-AUTH-RECORD, UPDATE-AUTH-DETAILS, ROLL-BACK, POPULATE-AUTH-DETAILS

### PROCESS-PF8-KEY

This paragraph facilitates sequential navigation through authorization records for a specific account. It first reads the current record to establish position in the IMS database and then calls READ-NEXT-AUTH-RECORD to fetch the subsequent child segment. If the end of the database is reached, it sets a message for the user. If a record is found, it updates the selection key in the COMMAREA and calls POPULATE-AUTH-DETAILS to refresh the screen with the new record's data.

**Calls:** READ-AUTH-RECORD, READ-NEXT-AUTH-RECORD, TAKE-SYNCPOINT, POPULATE-AUTH-DETAILS

### POPULATE-AUTH-DETAILS

This paragraph maps the raw data from the IMS PENDING-AUTH-DETAILS segment into the BMS map fields for display. It performs several data transformations, including formatting dates (YYMMDD to MM/DD/YY) and times (HHMMSS to HH:MM:SS). It also includes logic to set screen attributes, such as coloring the response code green for approvals and red for declines. It uses a SEARCH ALL statement against a hardcoded reason table to translate numeric decline codes into human-readable descriptions.

### READ-AUTH-RECORD

This paragraph performs the physical I/O to the IMS database to retrieve a specific authorization. It first ensures the PSB is scheduled. It then issues a 'GU' (Get Unique) call to the root segment (PAUTSUM0) using the Account ID. If the root is found, it issues a 'GNP' (Get Next within Parent) call to find the specific child segment (PAUTDTL1) matching the Authorization Key. It handles various IMS status codes, setting EOF flags or error messages as appropriate.

**Calls:** SCHEDULE-PSB, SEND-AUTHVIEW-SCREEN

### UPDATE-AUTH-DETAILS

This paragraph updates an existing authorization record in the IMS database. It moves the updated fraud information into the segment layout and issues an IMS 'REPL' (Replace) command for the PAUTDTL1 segment. If the update is successful, it calls TAKE-SYNCPOINT to commit the transaction and sets a success message. If the update fails for any reason, it triggers a ROLL-BACK to ensure data integrity and reports the system error to the user.

**Calls:** TAKE-SYNCPOINT, ROLL-BACK, SEND-AUTHVIEW-SCREEN

## Data Flow

### Reads From

- **PAUTDTL1:** PA-CARD-NUM, PA-APPROVED-AMT, PA-MERCHANT-NAME

### Writes To

- **COPAU1AO:** CARDNUMO, AUTHDTO, AUTHAMTO

### Transforms

- `PA-AUTH-ORIG-DATE` -> `AUTHDTO`: Reformatting YYMMDD to MM/DD/YY for display.

## Error Handling

- **IMS Status Code NOT OK:** Set error flag, format error message with status code, and display on screen.
- **CICS Link Failure or Fraud Update Failure:** Perform SYNCPOINT ROLLBACK to undo database changes.

## CICS Operations

- LINK
- SEND MAP
- SYNCPOINT
- SYNCPOINT ROLLBACK

## Flow Diagram

```mermaid
flowchart TD
    %% Title: COPAUS1C.cbl
    MAIN_PARA["MAIN-PARA"]
    MARK_AUTH_FRAUD["MARK-AUTH-FRAUD"]
    PROCESS_ENTER_KEY["PROCESS-ENTER-KEY"]
    PROCESS_PF8_KEY["PROCESS-PF8-KEY"]
    RECEIVE_AUTHVIEW_SCREEN["RECEIVE-AUTHVIEW-SCREEN"]
    RETURN_TO_PREV_SCREEN["RETURN-TO-PREV-SCREEN"]
    SEND_AUTHVIEW_SCREEN["SEND-AUTHVIEW-SCREEN"]
    POPULATE_AUTH_DETAILS["POPULATE-AUTH-DETAILS"]
    READ_AUTH_RECORD["READ-AUTH-RECORD"]
    ROLL_BACK["ROLL-BACK"]
    UPDATE_AUTH_DETAILS["UPDATE-AUTH-DETAILS"]
    WS_PGM_AUTH_FRAUD__ext(["WS-PGM-AUTH-FRAUD"])
    POPULATE_HEADER_INFO["POPULATE-HEADER-INFO"]
    TAKE_SYNCPOINT["TAKE-SYNCPOINT"]
    READ_NEXT_AUTH_RECORD["READ-NEXT-AUTH-RECORD"]
    SCHEDULE_PSB["SCHEDULE-PSB"]
    CDEMO_TO_PROGRAM__ext(["CDEMO-TO-PROGRAM"])
    MAIN_PARA --> MARK_AUTH_FRAUD
    MAIN_PARA --> PROCESS_ENTER_KEY
    MAIN_PARA --> PROCESS_PF8_KEY
    MAIN_PARA --> RECEIVE_AUTHVIEW_SCREEN
    MAIN_PARA --> RETURN_TO_PREV_SCREEN
    MAIN_PARA --> SEND_AUTHVIEW_SCREEN
    MARK_AUTH_FRAUD --> POPULATE_AUTH_DETAILS
    MARK_AUTH_FRAUD --> READ_AUTH_RECORD
    MARK_AUTH_FRAUD --> ROLL_BACK
    MARK_AUTH_FRAUD --> UPDATE_AUTH_DETAILS
    MARK_AUTH_FRAUD -.->|calls| WS_PGM_AUTH_FRAUD__ext
    PROCESS_ENTER_KEY --> POPULATE_AUTH_DETAILS
    PROCESS_ENTER_KEY --> READ_AUTH_RECORD
    PROCESS_ENTER_KEY --> TAKE_SYNCPOINT
    PROCESS_PF8_KEY --> POPULATE_AUTH_DETAILS
    PROCESS_PF8_KEY --> READ_AUTH_RECORD
    PROCESS_PF8_KEY --> READ_NEXT_AUTH_RECORD
    PROCESS_PF8_KEY --> TAKE_SYNCPOINT
    READ_AUTH_RECORD --> SCHEDULE_PSB
    READ_AUTH_RECORD --> SEND_AUTHVIEW_SCREEN
    READ_NEXT_AUTH_RECORD --> SEND_AUTHVIEW_SCREEN
    RETURN_TO_PREV_SCREEN -.->|calls| CDEMO_TO_PROGRAM__ext
    SCHEDULE_PSB --> SEND_AUTHVIEW_SCREEN
    SEND_AUTHVIEW_SCREEN --> POPULATE_HEADER_INFO
    UPDATE_AUTH_DETAILS --> ROLL_BACK
    UPDATE_AUTH_DETAILS --> SEND_AUTHVIEW_SCREEN
    UPDATE_AUTH_DETAILS --> TAKE_SYNCPOINT
```