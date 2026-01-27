# COPAUS1C

**File**: `cbl/COPAUS1C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-27 23:05:53.974206

## Purpose

COPAUS1C is a CICS COBOL program that displays detailed information about a selected authorization record. It retrieves authorization details from an IMS database and presents them on a CICS screen, allowing users to mark authorizations as fraudulent or navigate to the next authorization record.

**Business Context**: This program is part of a card authorization demo application, providing a detailed view of authorization messages for review and fraud management. It allows users to investigate specific authorization events and flag them for further investigation if necessary.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Data passed from the calling program, containing account ID, customer ID, and selected authorization key, as well as program context. |
| PAUTSUM0 | IOType.IMS_SEGMENT | Pending Authorization Summary segment from the IMS database, containing summary information about the authorization. |
| PAUTDTL1 | IOType.IMS_SEGMENT | Pending Authorization Details segment from the IMS database, containing detailed information about the authorization. |
| COPAU1AI | IOType.CICS_MAP | Input map for the COPAU1A screen, used to receive user input. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output map for the COPAU1A screen, used to display authorization details and messages to the user. |
| WS-FRAUD-DATA | IOType.CICS_COMMAREA | Data passed to COPAUS2C program, containing fraud data. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS2C | CallType.CICS_LINK | To update the fraud status of the authorization record. |

## Business Rules

- **BR001**: If the account ID is numeric and an authorization key is selected, read the authorization record from the IMS database.
- **BR002**: Determine the authorization response status and color code.

## Paragraphs/Procedures

### MAIN-PARA
This is the main paragraph that controls the program's overall flow. It first initializes error flags and the WS-MESSAGE field. It checks if the COMMAREA is empty, indicating the program is being called for the first time. If the COMMAREA is empty, it initializes the CARDDEMO-COMMAREA and calls RETURN-TO-PREV-SCREEN to return to the authorization summary screen. If the COMMAREA is not empty, it moves the data from the COMMAREA to the CARDDEMO-COMMAREA and checks if the program is being re-entered. If it's not a re-entry, it performs PROCESS-ENTER-KEY and SEND-AUTHVIEW-SCREEN to display the initial authorization details. If it's a re-entry, it receives the screen data, evaluates the EIBAID to determine the user's action (ENTER, PF3, PF5, PF8, or other), and performs the corresponding action. Finally, it returns control to CICS with the updated COMMAREA.

### PROCESS-ENTER-KEY
This paragraph processes the ENTER key press. It initializes the COPAU1AO map structure with low-values. It checks if the account ID (CDEMO-ACCT-ID) is numeric and if an authorization key (CDEMO-CPVD-PAU-SELECTED) has been selected. If both conditions are met, it moves the account ID and authorization key to working storage variables (WS-ACCT-ID and WS-AUTH-KEY) and calls READ-AUTH-RECORD to retrieve the authorization details from the IMS database. If the PSB is scheduled, it sets the IMS-PSB-NOT-SCHD flag to TRUE and performs TAKE-SYNCPOINT. If either condition is not met, it sets the ERR-FLG-ON flag to TRUE. Finally, it calls POPULATE-AUTH-DETAILS to populate the screen fields with the retrieved authorization details.

### MARK-AUTH-FRAUD
This paragraph handles the process of marking an authorization as fraudulent. It moves the account ID and selected authorization key to working storage. It calls READ-AUTH-RECORD to retrieve the authorization details. It checks if the authorization is already marked as fraud confirmed. If it is, it sets the PA-FRAUD-REMOVED flag to TRUE and WS-REMOVE-FRAUD to TRUE, indicating the fraud flag should be removed. Otherwise, it sets PA-FRAUD-CONFIRMED to TRUE and WS-REPORT-FRAUD to TRUE, indicating the authorization should be marked as fraudulent. It then moves the PENDING-AUTH-DETAILS to WS-FRAUD-AUTH-RECORD and the account and customer IDs to their respective working storage fields. It links to the COPAUS2C program, passing the WS-FRAUD-DATA COMMAREA. After the link, it checks the EIBRESP and WS-FRD-UPDT-SUCCESS flags. If the update was successful, it calls UPDATE-AUTH-DETAILS. Otherwise, it moves the error message to WS-MESSAGE and performs a ROLL-BACK. Finally, it moves the authorization key to CDEMO-CPVD-PAU-SELECTED and calls POPULATE-AUTH-DETAILS to refresh the screen.

### PROCESS-PF8-KEY
This paragraph handles the processing when the PF8 key is pressed, which is intended to navigate to the next authorization record. It moves the account ID and selected authorization key to working storage. It calls READ-AUTH-RECORD to retrieve the current authorization record and then calls READ-NEXT-AUTH-RECORD to retrieve the next authorization record. If the PSB is scheduled, it sets the IMS-PSB-NOT-SCHD flag to TRUE and performs TAKE-SYNCPOINT. If the end of the authorization records is reached (AUTHS-EOF is TRUE), it sets the SEND-ERASE-NO flag to TRUE and moves a message indicating that the last authorization has been reached to WS-MESSAGE. Otherwise, it moves the authorization key to CDEMO-CPVD-PAU-SELECTED and calls POPULATE-AUTH-DETAILS to display the next authorization record's details.

### POPULATE-AUTH-DETAILS
This paragraph populates the output map (COPAU1AO) with the authorization details retrieved from the IMS database. It first checks if the ERR-FLG-OFF flag is TRUE. If it is, it moves various fields from the PENDING-AUTH-DETAILS segment to the corresponding output fields in the COPAU1AO map. This includes the card number, authorization date and time, approved amount, authorization response code and reason, processing code, POS entry mode, message source, merchant category code, card expiry date, authorization type, transaction ID, match status, fraud information, and merchant details. It performs a SEARCH on the WS-DECLINE-REASON-TAB to find the description corresponding to the authorization response reason code. If the decline reason is not found, it moves an error message to the AUTHRSNO field. If ERR-FLG-OFF is not TRUE, this paragraph does nothing.

### RETURN-TO-PREV-SCREEN
This paragraph prepares the COMMAREA for returning to the previous screen (authorization summary). It moves the CICS transaction ID (WS-CICS-TRANID) to the CDEMO-FROM-TRANID field and the program ID (WS-PGM-AUTH-DTL) to the CDEMO-FROM-PROGRAM field. It sets the program context (CDEMO-PGM-CONTEXT) to ZEROS and the CDEMO-PGM-ENTER flag to TRUE. Finally, it performs a CICS XCTL to the program specified in CDEMO-TO-PROGRAM, passing the CARDDEMO-COMMAREA.

### SEND-AUTHVIEW-SCREEN
This paragraph sends the authorization details screen to the terminal. It first calls POPULATE-HEADER-INFO to populate the header information on the screen. It moves the WS-MESSAGE to the ERRMSGO field in the COPAU1AO map to display any error messages. It moves -1 to CARDNUML. It then checks the SEND-ERASE-YES flag. If it's TRUE, it executes a CICS SEND command with the ERASE option to clear the screen before sending the map. Otherwise, it executes a CICS SEND command without the ERASE option. The map is COPAU1A and the mapset is COPAU01, and the data is sent from the COPAU1AO map.

### RECEIVE-AUTHVIEW-SCREEN
This paragraph receives data from the COPAU1A screen. It executes a CICS RECEIVE command to receive the data from the COPAU1A map in the COPAU01 mapset into the COPAU1AI structure. The NOHANDLE option is used to suppress exception handling.

### POPULATE-HEADER-INFO
This paragraph populates the header information in the COPAU1AO map. It moves the current date to WS-CURDATE-DATA using the FUNCTION CURRENT-DATE. It then moves the title strings (CCDA-TITLE01 and CCDA-TITLE02) and the transaction and program names (WS-CICS-TRANID and WS-PGM-AUTH-DTL) to their respective fields in the COPAU1AO map. It formats the current date and time and moves them to the CURDATEO and CURTIMEO fields in the COPAU1AO map.

### READ-AUTH-RECORD
This paragraph reads the authorization record from the IMS database. It first calls SCHEDULE-PSB to schedule the PSB. It then moves the account ID (WS-ACCT-ID) and authorization key (WS-AUTH-KEY) to the corresponding fields in the PENDING-AUTH-SUMMARY segment. It executes a DLI GU (Get Unique) command to retrieve the PENDING-AUTH-SUMMARY segment from the IMS database, using the account ID as the search criteria. It then moves the DIBSTAT value to IMS-RETURN-CODE and evaluates the return code. If the status is OK, it sets AUTHS-NOT-EOF to TRUE. If the segment is not found or the end of the database is reached, it sets AUTHS-EOF to TRUE. If any other error occurs, it sets the WS-ERR-FLG to 'Y', constructs an error message, and calls SEND-AUTHVIEW-SCREEN to display the error message. If the PENDING-AUTH-SUMMARY segment is successfully retrieved, it executes a DLI GNP (Get Next within Parent) command to retrieve the PENDING-AUTH-DETAILS segment, using the authorization key as the search criteria. It then moves the DIBSTAT value to IMS-RETURN-CODE and evaluates the return code, handling errors similarly to the summary segment retrieval.

### READ-NEXT-AUTH-RECORD
This paragraph reads the next authorization detail record from the IMS database. It executes a DLI GNP (Get Next within Parent) command to retrieve the next PENDING-AUTH-DETAILS segment. It then moves the DIBSTAT value to IMS-RETURN-CODE and evaluates the return code. If the status is OK, it sets AUTHS-NOT-EOF to TRUE. If the segment is not found or the end of the database is reached, it sets AUTHS-EOF to TRUE. If any other error occurs, it sets the WS-ERR-FLG to 'Y', constructs an error message, and calls SEND-AUTHVIEW-SCREEN to display the error message.

### UPDATE-AUTH-DETAILS
This paragraph updates the authorization details in the IMS database. It moves the WS-FRAUD-AUTH-RECORD (containing the updated fraud information) to the PENDING-AUTH-DETAILS segment. It then executes a DLI REPL (Replace) command to update the PENDING-AUTH-DETAILS segment in the IMS database. It moves the DIBSTAT value to IMS-RETURN-CODE and evaluates the return code. If the status is OK, it performs TAKE-SYNCPOINT to commit the changes and moves a message indicating whether the authorization was marked or removed as fraud to WS-MESSAGE. Otherwise, it performs ROLL-BACK to undo the changes, sets the WS-ERR-FLG to 'Y', constructs an error message, and calls SEND-AUTHVIEW-SCREEN to display the error message.

### TAKE-SYNCPOINT
This paragraph issues a CICS SYNCPOINT command to commit any changes made to recoverable resources, such as the IMS database. This ensures data consistency and prevents data loss in case of a system failure.

### ROLL-BACK
This paragraph issues a CICS SYNCPOINT ROLLBACK command to undo any changes made to recoverable resources since the last SYNCPOINT. This is used to ensure data consistency in case of an error during processing.

### SCHEDULE-PSB
This paragraph schedules the PSB (Program Specification Block) for IMS database access. It executes a DLI SCHD command to schedule the PSB specified in PSB-NAME. If the PSB has already been scheduled, it terminates the current PSB and schedules it again. If the scheduling is successful, it sets the IMS-PSB-SCHD flag to TRUE. Otherwise, it sets the WS-ERR-FLG to 'Y', constructs an error message, and calls SEND-AUTHVIEW-SCREEN to display the error message.

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

- ? What is the exact purpose and structure of the IMS database being accessed?
  - Context: The program uses DLI calls to access an IMS database, but the specific database schema and segment relationships are not fully clear from the code. Understanding the database structure would provide a more complete picture of the program's data flow.
- ? What is the specific functionality of the COPAUS2C program?
  - Context: The program links to COPAUS2C to update the fraud status of the authorization record, but the internal workings of COPAUS2C are not visible in this code. Understanding COPAUS2C's functionality would provide a more complete picture of the fraud processing logic.
