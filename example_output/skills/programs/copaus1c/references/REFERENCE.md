# COPAUS1C - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** COPAUS1C
- **File Name:** COPAUS1C.cbl
- **File Type:** COBOL
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:55:16.220421

## Purpose

**Summary:** COPAUS1C is a CICS COBOL program that displays detailed information about an authorization message. It retrieves authorization details from an IMS database, allows users to mark authorizations as fraudulent, and links to another program (COPAUS2C) to handle fraud updates.

**Business Context:** This program is part of a card authorization demo application, providing a detailed view of authorization messages for review and potential fraud flagging.
**Program Type:** ONLINE_CICS

## Inputs

### CARDDEMO-COMMAREA

- **Type:** CICS_COMMAREA
- **Description:** Communication area passed between CICS programs, containing account ID, authorization keys, and other relevant data.
- **Copybook:** COCOM01Y

### PENDING-AUTH-SUMMARY

- **Type:** IMS_SEGMENT
- **Description:** IMS segment containing summary information about a pending authorization.
- **Copybook:** CIPAUSMY

### PENDING-AUTH-DETAILS

- **Type:** IMS_SEGMENT
- **Description:** IMS segment containing detailed information about a pending authorization.
- **Copybook:** CIPAUDTY

## Outputs

### COPAU1A

- **Type:** CICS_MAP
- **Description:** BMS map displaying authorization details to the user.
- **Copybook:** COPAU01

### WS-FRAUD-DATA

- **Type:** CICS_COMMAREA
- **Description:** Communication area passed to COPAUS2C containing fraud related data.

## Business Rules

### BR001

**Description:** Authorization response codes are translated to 'A' (Approved) or 'D' (Declined) for display.

**Logic:** If PA-AUTH-RESP-CODE is '00', AUTHRSPO is set to 'A' and the color is set to green. Otherwise, AUTHRSPO is set to 'D' and the color is set to red.

**Conditions:**
- `IF PA-AUTH-RESP-CODE = '00'`

### BR002

**Description:** Decline reason codes are translated to a user-friendly description using a table lookup.

**Logic:** The program searches WS-DECLINE-REASON-TAB for a matching DECL-CODE. If found, the corresponding DECL-DESC is displayed. If not found, a default error message is displayed.

**Conditions:**
- `WHEN DECL-CODE(WS-DECL-RSN-IDX) = PA-AUTH-RESP-REASON`

## Paragraphs

### MAIN-PARA

This is the main paragraph that controls the program's overall flow. It first initializes variables and checks if the program is being called for the first time or re-entered. If it's the first time, it initializes the CARDDEMO-COMMAREA and returns to the previous screen (COPAUS0C). If it's a re-entry, it receives the screen input, evaluates the EIBAID (CICS attention identifier) to determine the user's action (ENTER, PF3, PF5, PF8, or other), and performs the corresponding action. Based on the AID key pressed, it will either process the enter key, return to the previous screen, mark the authorization as fraud, process the PF8 key to read the next authorization record, or display an error message for an invalid key. Finally, it returns control to CICS with the updated COMMAREA.

**Calls:** RETURN-TO-PREV-SCREEN, PROCESS-ENTER-KEY, SEND-AUTHVIEW-SCREEN, RECEIVE-AUTHVIEW-SCREEN, MARK-AUTH-FRAUD, PROCESS-PF8-KEY

### PROCESS-ENTER-KEY

This paragraph processes the ENTER key press. It initializes the COPAU1AO map and checks if the account ID and authorization key are valid (numeric and not spaces/low-values). If valid, it moves the account ID and authorization key to working storage variables and performs READ-AUTH-RECORD to retrieve the authorization details from the IMS database. If the PSB is scheduled, it takes a syncpoint. If the account ID or authorization key are invalid, it sets the error flag. Finally, it calls POPULATE-AUTH-DETAILS to populate the screen fields with the retrieved data.

**Calls:** READ-AUTH-RECORD, POPULATE-AUTH-DETAILS, TAKE-SYNCPOINT

### MARK-AUTH-FRAUD

This paragraph handles the process of marking or unmarking an authorization as fraudulent. It moves the account ID and authorization key to working storage and calls READ-AUTH-RECORD to retrieve the authorization details. It then checks if the authorization is already marked as fraud. If it is, it removes the fraud flag; otherwise, it sets the fraud flag. It moves the PENDING-AUTH-DETAILS to WS-FRAUD-AUTH-RECORD, the account ID to WS-FRD-ACCT-ID, and the customer ID to WS-FRD-CUST-ID. It then links to the COPAUS2C program to update the fraud status. Based on the return from COPAUS2C, it either updates the authorization details and takes a syncpoint or rolls back the changes and displays an error message. Finally, it calls POPULATE-AUTH-DETAILS to refresh the screen with the updated information.

**Calls:** READ-AUTH-RECORD, POPULATE-AUTH-DETAILS, UPDATE-AUTH-DETAILS, ROLL-BACK

### PROCESS-PF8-KEY

This paragraph handles the processing when the PF8 key is pressed, which is intended to display the next authorization record. It moves the account ID and authorization key to working storage and calls READ-AUTH-RECORD to retrieve the current authorization details. It then calls READ-NEXT-AUTH-RECORD to retrieve the next authorization record from the IMS database. If the PSB is scheduled, it takes a syncpoint. If the end of the authorization records is reached (AUTHS-EOF), it sets a flag to prevent screen erasure and displays a message indicating that the last authorization has been reached. Otherwise, it moves the authorization key of the next record to CDEMO-CPVD-PAU-SELECTED and calls POPULATE-AUTH-DETAILS to display the details of the next authorization record.

**Calls:** READ-AUTH-RECORD, READ-NEXT-AUTH-RECORD, POPULATE-AUTH-DETAILS, TAKE-SYNCPOINT

### POPULATE-AUTH-DETAILS

This paragraph populates the authorization details on the screen (COPAU1AO) based on the data retrieved from the IMS database. It checks if there are any errors (ERR-FLG-OFF). If no errors, it moves various fields from the PENDING-AUTH-DETAILS segment to the corresponding output fields on the screen, formatting the date and time as needed. It translates the authorization response code to 'A' (Approved) or 'D' (Declined) and sets the corresponding color. It also searches the WS-DECLINE-REASON-TAB to find the description for the decline reason code. It handles fraud confirmation status and populates the merchant information. If there are errors, it skips populating the screen fields.

### RETURN-TO-PREV-SCREEN

This paragraph prepares the COMMAREA for returning to the previous screen (COPAUS0C) and then transfers control to that program using CICS XCTL. It moves the current transaction ID and program ID to the CARDDEMO-COMMAREA, sets the program context to zero, and sets the program enter flag to true. It then executes the CICS XCTL command to transfer control to the program specified in CDEMO-TO-PROGRAM, passing the CARDDEMO-COMMAREA.

### SEND-AUTHVIEW-SCREEN

This paragraph sends the authorization view screen (COPAU1A) to the terminal. It first calls POPULATE-HEADER-INFO to populate the header information on the screen. It then moves any error messages to the ERRMSGO field on the screen. It checks the SEND-ERASE-FLG to determine whether to erase the screen before sending the map. If SEND-ERASE-YES is true, it sends the map with the ERASE option; otherwise, it sends the map without erasing the screen. The CURSOR option is used to position the cursor on the screen.

**Calls:** POPULATE-HEADER-INFO

### RECEIVE-AUTHVIEW-SCREEN

This paragraph receives data from the authorization view screen (COPAU1A) when the user interacts with it. It executes the CICS RECEIVE command to receive the data from the specified map and mapset into the COPAU1AI structure. The NOHANDLE option is used to prevent CICS from handling any exceptions that may occur during the receive operation.

### POPULATE-HEADER-INFO

This paragraph populates the header information on the authorization view screen (COPAU1AO). It moves the current date and time to working storage variables. It then moves the title, transaction name, program name, current date, and current time to the corresponding output fields on the screen.

### READ-AUTH-RECORD

This paragraph reads the authorization summary and detail records from the IMS database. It first calls SCHEDULE-PSB to schedule the PSB. It then moves the account ID and authorization key to the corresponding fields in the PENDING-AUTH-SUMMARY segment. It executes a DLI GU (Get Unique) command to retrieve the PENDING-AUTH-SUMMARY segment based on the account ID. If the segment is found, it executes a DLI GNP (Get Next within Parent) command to retrieve the PENDING-AUTH-DETAILS segment based on the authorization key. Error handling is performed by checking the DIBSTAT value after each DLI call. If an error occurs, an error message is constructed and displayed on the screen.

**Calls:** SCHEDULE-PSB, SEND-AUTHVIEW-SCREEN

### READ-NEXT-AUTH-RECORD

This paragraph reads the next authorization detail record from the IMS database. It executes a DLI GNP (Get Next within Parent) command to retrieve the next PENDING-AUTH-DETAILS segment. Error handling is performed by checking the DIBSTAT value after the DLI call. If an error occurs, an error message is constructed and displayed on the screen. The AUTHS-EOF flag is set if the end of the database is reached.

**Calls:** SEND-AUTHVIEW-SCREEN

### UPDATE-AUTH-DETAILS

This paragraph updates the PENDING-AUTH-DETAILS segment in the IMS database with the fraud information. It moves the WS-FRAUD-AUTH-RECORD to the PENDING-AUTH-DETAILS segment. It then executes a DLI REPL (Replace) command to update the segment in the database. Error handling is performed by checking the DIBSTAT value after the DLI call. If the update is successful, it takes a syncpoint and displays a message indicating whether the authorization was marked or removed as fraud. If an error occurs, it rolls back the changes and displays an error message.

**Calls:** TAKE-SYNCPOINT, ROLL-BACK, SEND-AUTHVIEW-SCREEN

### TAKE-SYNCPOINT

This paragraph takes a CICS syncpoint to commit the changes made to the database. It executes the CICS SYNCPOINT command.

### ROLL-BACK

This paragraph rolls back any uncommitted changes made to the database. It executes the CICS SYNCPOINT ROLLBACK command.

### SCHEDULE-PSB

This paragraph schedules the PSB (Program Specification Block) for IMS database access. It executes a DLI SCHD (Schedule) command to schedule the PSB named in PSB-NAME. It checks the DIBSTAT value after the DLI call. If the PSB was already scheduled, it terminates the PSB and schedules it again. If an error occurs during scheduling, an error message is constructed and displayed on the screen.

**Calls:** SEND-AUTHVIEW-SCREEN

## Data Flow

### Reads From

- **CARDDEMO-COMMAREA:** CDEMO-ACCT-ID, CDEMO-CPVD-PAU-SELECTED, CDEMO-PGM-REENTER, CDEMO-TO-PROGRAM, CDEMO-CUST-ID
- **PENDING-AUTH-SUMMARY:** PA-ACCT-ID, PA-AUTHORIZATION-KEY
- **PENDING-AUTH-DETAILS:** PA-CARD-NUM, PA-AUTH-ORIG-DATE, PA-AUTH-ORIG-TIME, PA-APPROVED-AMT, PA-AUTH-RESP-CODE, PA-AUTH-RESP-REASON, PA-PROCESSING-CODE, PA-POS-ENTRY-MODE, PA-MESSAGE-SOURCE, PA-MERCHANT-CATAGORY-CODE, PA-CARD-EXPIRY-DATE, PA-AUTH-TYPE, PA-TRANSACTION-ID, PA-MATCH-STATUS, PA-AUTH-FRAUD, PA-FRAUD-RPT-DATE, PA-MERCHANT-NAME, PA-MERCHANT-ID, PA-MERCHANT-CITY, PA-MERCHANT-STATE, PA-MERCHANT-ZIP, PA-FRAUD-CONFIRMED, PA-FRAUD-REMOVED

### Writes To

- **COPAU1AO:** CARDNUMO, AUTHDTO, AUTHTMO, AUTHAMTO, AUTHRSPO, AUTHRSPC, AUTHRSNO, AUTHCDO, POSEMDO, AUTHSRCO, MCCCDO, CRDEXPO, AUTHTYPO, TRNIDO, AUTHMTCO, AUTHFRDO, MERNAMEO, MERIDO, MERCITYO, MERSTO, MERZIPO, TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO, ERRMSGO
- **WS-FRAUD-DATA:** WS-FRD-ACCT-ID, WS-FRD-CUST-ID, WS-FRAUD-AUTH-RECORD

### Transforms

- `PA-AUTH-ORIG-DATE` -> `AUTHDTO`: Formats the authorization date from YYMMDD to MM/DD/YY.
- `PA-AUTH-ORIG-TIME` -> `AUTHTMO`: Formats the authorization time from HHMMSS to HH:MM:SS.
- `PA-APPROVED-AMT` -> `AUTHAMTO`: Moves the approved amount to the output field.

## Error Handling

- **EIBCALEN = 0:** Initializes CARDDEMO-COMMAREA and returns to the previous screen.
- **CDEMO-ACCT-ID is not NUMERIC or CDEMO-CPVD-PAU-SELECTED is SPACES or LOW-VALUES:** Sets ERR-FLG-ON to TRUE.
- **EIBRESP not equal to DFHRESP(NORMAL) during LINK to COPAUS2C:** Performs ROLL-BACK.
- **WS-FRD-UPDT-SUCCESS is false after LINK to COPAUS2C:** Moves WS-FRD-ACT-MSG to WS-MESSAGE and performs ROLL-BACK.
- **DIBSTAT indicates an error during IMS operations (READ-AUTH-RECORD, READ-NEXT-AUTH-RECORD, UPDATE-AUTH-DETAILS, SCHEDULE-PSB):** Sets WS-ERR-FLG to 'Y', constructs an error message, and displays it on the screen.

## CICS Operations

- RETURN
- XCTL
- SEND MAP
- RECEIVE MAP
- LINK
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