# COPAUS1C

**File**: `cbl/COPAUS1C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-28 14:55:16.220421

## Purpose

COPAUS1C is a CICS COBOL program that displays detailed information about an authorization message. It retrieves authorization details from an IMS database, allows users to mark authorizations as fraudulent, and links to another program (COPAUS2C) to handle fraud updates.

**Business Context**: This program is part of a card authorization demo application, providing a detailed view of authorization messages for review and potential fraud flagging.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Communication area passed between CICS programs, containing account ID, authorization keys, and other relevant data. |
| PENDING-AUTH-SUMMARY | IOType.IMS_SEGMENT | IMS segment containing summary information about a pending authorization. |
| PENDING-AUTH-DETAILS | IOType.IMS_SEGMENT | IMS segment containing detailed information about a pending authorization. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | BMS map displaying authorization details to the user. |
| WS-FRAUD-DATA | IOType.CICS_COMMAREA | Communication area passed to COPAUS2C containing fraud related data. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS0C | CallType.CICS_XCTL | Returns to the previous screen (Authorization Summary). |
| COPAUS2C | CallType.CICS_LINK | Handles fraud reporting and updating. |

## Business Rules

- **BR001**: Authorization response codes are translated to 'A' (Approved) or 'D' (Declined) for display.
- **BR002**: Decline reason codes are translated to a user-friendly description using a table lookup.

## Paragraphs/Procedures

### MAIN-PARA
This is the main paragraph that controls the program's overall flow. It first initializes variables and checks if the program is being called for the first time or re-entered. If it's the first time, it initializes the CARDDEMO-COMMAREA and returns to the previous screen (COPAUS0C). If it's a re-entry, it receives the screen input, evaluates the EIBAID (CICS attention identifier) to determine the user's action (ENTER, PF3, PF5, PF8, or other), and performs the corresponding action. Based on the AID key pressed, it will either process the enter key, return to the previous screen, mark the authorization as fraud, process the PF8 key to read the next authorization record, or display an error message for an invalid key. Finally, it returns control to CICS with the updated COMMAREA.

### PROCESS-ENTER-KEY
This paragraph processes the ENTER key press. It initializes the COPAU1AO map and checks if the account ID and authorization key are valid (numeric and not spaces/low-values). If valid, it moves the account ID and authorization key to working storage variables and performs READ-AUTH-RECORD to retrieve the authorization details from the IMS database. If the PSB is scheduled, it takes a syncpoint. If the account ID or authorization key are invalid, it sets the error flag. Finally, it calls POPULATE-AUTH-DETAILS to populate the screen fields with the retrieved data.

### MARK-AUTH-FRAUD
This paragraph handles the process of marking or unmarking an authorization as fraudulent. It moves the account ID and authorization key to working storage and calls READ-AUTH-RECORD to retrieve the authorization details. It then checks if the authorization is already marked as fraud. If it is, it removes the fraud flag; otherwise, it sets the fraud flag. It moves the PENDING-AUTH-DETAILS to WS-FRAUD-AUTH-RECORD, the account ID to WS-FRD-ACCT-ID, and the customer ID to WS-FRD-CUST-ID. It then links to the COPAUS2C program to update the fraud status. Based on the return from COPAUS2C, it either updates the authorization details and takes a syncpoint or rolls back the changes and displays an error message. Finally, it calls POPULATE-AUTH-DETAILS to refresh the screen with the updated information.

### PROCESS-PF8-KEY
This paragraph handles the processing when the PF8 key is pressed, which is intended to display the next authorization record. It moves the account ID and authorization key to working storage and calls READ-AUTH-RECORD to retrieve the current authorization details. It then calls READ-NEXT-AUTH-RECORD to retrieve the next authorization record from the IMS database. If the PSB is scheduled, it takes a syncpoint. If the end of the authorization records is reached (AUTHS-EOF), it sets a flag to prevent screen erasure and displays a message indicating that the last authorization has been reached. Otherwise, it moves the authorization key of the next record to CDEMO-CPVD-PAU-SELECTED and calls POPULATE-AUTH-DETAILS to display the details of the next authorization record.

### POPULATE-AUTH-DETAILS
This paragraph populates the authorization details on the screen (COPAU1AO) based on the data retrieved from the IMS database. It checks if there are any errors (ERR-FLG-OFF). If no errors, it moves various fields from the PENDING-AUTH-DETAILS segment to the corresponding output fields on the screen, formatting the date and time as needed. It translates the authorization response code to 'A' (Approved) or 'D' (Declined) and sets the corresponding color. It also searches the WS-DECLINE-REASON-TAB to find the description for the decline reason code. It handles fraud confirmation status and populates the merchant information. If there are errors, it skips populating the screen fields.

### RETURN-TO-PREV-SCREEN
This paragraph prepares the COMMAREA for returning to the previous screen (COPAUS0C) and then transfers control to that program using CICS XCTL. It moves the current transaction ID and program ID to the CARDDEMO-COMMAREA, sets the program context to zero, and sets the program enter flag to true. It then executes the CICS XCTL command to transfer control to the program specified in CDEMO-TO-PROGRAM, passing the CARDDEMO-COMMAREA.

### SEND-AUTHVIEW-SCREEN
This paragraph sends the authorization view screen (COPAU1A) to the terminal. It first calls POPULATE-HEADER-INFO to populate the header information on the screen. It then moves any error messages to the ERRMSGO field on the screen. It checks the SEND-ERASE-FLG to determine whether to erase the screen before sending the map. If SEND-ERASE-YES is true, it sends the map with the ERASE option; otherwise, it sends the map without erasing the screen. The CURSOR option is used to position the cursor on the screen.

### RECEIVE-AUTHVIEW-SCREEN
This paragraph receives data from the authorization view screen (COPAU1A) when the user interacts with it. It executes the CICS RECEIVE command to receive the data from the specified map and mapset into the COPAU1AI structure. The NOHANDLE option is used to prevent CICS from handling any exceptions that may occur during the receive operation.

### POPULATE-HEADER-INFO
This paragraph populates the header information on the authorization view screen (COPAU1AO). It moves the current date and time to working storage variables. It then moves the title, transaction name, program name, current date, and current time to the corresponding output fields on the screen.

### READ-AUTH-RECORD
This paragraph reads the authorization summary and detail records from the IMS database. It first calls SCHEDULE-PSB to schedule the PSB. It then moves the account ID and authorization key to the corresponding fields in the PENDING-AUTH-SUMMARY segment. It executes a DLI GU (Get Unique) command to retrieve the PENDING-AUTH-SUMMARY segment based on the account ID. If the segment is found, it executes a DLI GNP (Get Next within Parent) command to retrieve the PENDING-AUTH-DETAILS segment based on the authorization key. Error handling is performed by checking the DIBSTAT value after each DLI call. If an error occurs, an error message is constructed and displayed on the screen.

### READ-NEXT-AUTH-RECORD
This paragraph reads the next authorization detail record from the IMS database. It executes a DLI GNP (Get Next within Parent) command to retrieve the next PENDING-AUTH-DETAILS segment. Error handling is performed by checking the DIBSTAT value after the DLI call. If an error occurs, an error message is constructed and displayed on the screen. The AUTHS-EOF flag is set if the end of the database is reached.

### UPDATE-AUTH-DETAILS
This paragraph updates the PENDING-AUTH-DETAILS segment in the IMS database with the fraud information. It moves the WS-FRAUD-AUTH-RECORD to the PENDING-AUTH-DETAILS segment. It then executes a DLI REPL (Replace) command to update the segment in the database. Error handling is performed by checking the DIBSTAT value after the DLI call. If the update is successful, it takes a syncpoint and displays a message indicating whether the authorization was marked or removed as fraud. If an error occurs, it rolls back the changes and displays an error message.

### TAKE-SYNCPOINT
This paragraph takes a CICS syncpoint to commit the changes made to the database. It executes the CICS SYNCPOINT command.

### ROLL-BACK
This paragraph rolls back any uncommitted changes made to the database. It executes the CICS SYNCPOINT ROLLBACK command.

### SCHEDULE-PSB
This paragraph schedules the PSB (Program Specification Block) for IMS database access. It executes a DLI SCHD (Schedule) command to schedule the PSB named in PSB-NAME. It checks the DIBSTAT value after the DLI call. If the PSB was already scheduled, it terminates the PSB and schedules it again. If an error occurs during scheduling, an error message is constructed and displayed on the screen.

## Control Flow

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

## Open Questions

- ? What is the purpose of the DISPLAY statement on line 523?
  - Context: It is unclear why the PA-FRAUD-RPT-DATE is being displayed to the console.
