# COPAUA0C

**File:** COPAUA0C.cbl
**Type:** COBOL
**Status:** In Progress
**Iterations:** 1
**Analyzed:** 2026-03-16 19:45:05.049515

## Purpose

This CICS COBOL program processes authorization requests received from an MQ queue, schedules an IMS PSB, processes the authorization, and then terminates. It retrieves the queue name and trigger data from the CICS terminal, opens the request queue, reads messages from the queue, processes them, and then terminates the connection.

**Business Context:** This program is likely part of a larger authorization processing system, handling requests for financial transactions or access to resources.
**Program Type:** ONLINE_CICS
**Citations:** Lines 5, 7, 9

## Inputs

### MQTM
- **Type:** CICS_COMMAREA
- **Description:** MQ Trigger Monitor data containing the queue name and trigger data.
- **Lines:** 19

### WS-REQUEST-QNAME
- **Type:** OTHER
- **Description:** The name of the MQ queue to read requests from.
- **Lines:** 42

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [MQOPEN](./MQOPEN.cbl.md) | STATIC_CALL | Opens the MQ request queue. | 46 |
| [MQGET](./MQGET.cbl.md) | STATIC_CALL | Reads a message from the MQ request queue. | 31 |
| [9500-LOG-ERROR](./9500-LOG-ERROR.cbl.md) | STATIC_CALL | Logs an error message. | 66 |

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [MQTM](../copybooks/MQTM.cpy.md) | LINKAGE | Defines the structure of the MQ Trigger Monitor data. | 19 |

## Data Flow

### Reads From
- **MQTM**: MQTM-QNAME, MQTM-TRIGGERDATA
  (Lines: 19, 23, 24)
- **WS-REQUEST-QNAME**: WS-REQUEST-QNAME
  (Lines: 42)

### Transformations
- **MQTM-QNAME** → **WS-REQUEST-QNAME**: Moves the queue name from the trigger monitor data to the working storage variable.
  (Lines: 23)
- **MQTM-TRIGGERDATA** → **WS-TRIGGER-DATA**: Moves the trigger data from the trigger monitor data to the working storage variable.
  (Lines: 24)
- **MQOT-Q** → **MQOD-OBJECTTYPE OF MQM-OD-REQUEST**: Moves the MQ object type to the MQ object descriptor.
  (Lines: 41)
- **WS-REQUEST-QNAME** → **MQOD-OBJECTNAME OF MQM-OD-REQUEST**: Moves the request queue name to the MQ object descriptor.
  (Lines: 42)

## Key Paragraphs

### MAIN-PARA
**Purpose:** This is the main control paragraph of the COPAUA0C program. It orchestrates the overall processing flow by calling other paragraphs in sequence. First, it calls 1000-INITIALIZE to perform initial setup tasks. Then, it calls 2000-MAIN-PROCESS to handle the core authorization processing logic. Finally, it calls 9000-TERMINATE to perform cleanup and termination procedures. After these calls, the program returns control to CICS. This paragraph does not directly handle any business logic or data manipulation, but rather acts as a high-level director of the program's execution.
- Calls: 1000-INITIALIZE, 2000-MAIN-PROCESS, 9000-TERMINATE
- Lines: 220-227

### 1000-INITIALIZE
**Purpose:** This paragraph performs the initialization tasks required for the COPAUA0C program. It starts by retrieving data from the CICS terminal using EXEC CICS RETRIEVE INTO(MQTM), which populates the MQTM structure with information about the MQ queue. It then moves the queue name and trigger data from MQTM to working storage variables WS-REQUEST-QNAME and WS-TRIGGER-DATA, respectively. Next, it sets the wait interval for MQ operations to 5000. Finally, it calls 1100-OPEN-REQUEST-QUEUE to open the MQ request queue and 3100-READ-REQUEST-MQ to read the first message from the queue. This paragraph sets up the environment for subsequent processing.
- Called by: MAIN-PARA
- Calls: 1100-OPEN-REQUEST-QUEUE, 3100-READ-REQUEST-MQ
- Lines: 230-247

### 1000-EXIT
**Purpose:** This paragraph serves as the exit point for the 1000-INITIALIZE paragraph. It simply contains the EXIT statement, which allows control to return to the calling paragraph (MAIN-PARA). It does not perform any specific actions or data manipulation. It is a standard practice in COBOL to use EXIT paragraphs to define the end of a PERFORM THRU sequence.
- Called by: MAIN-PARA, 1000-INITIALIZE
- Lines: 249-250

### 1100-OPEN-REQUEST-QUEUE
**Purpose:** This paragraph opens the MQ request queue for processing. It first moves the MQ object type (MQOT-Q) to the MQ object descriptor (MQOD-OBJECTTYPE OF MQM-OD-REQUEST) and the request queue name (WS-REQUEST-QNAME) to the MQ object name field in the descriptor (MQOD-OBJECTNAME OF MQM-OD-REQUEST). It then computes the options for opening the queue, setting WS-OPTIONS to MQOO-INPUT-SHARED. Next, it calls the MQOPEN API to open the queue, passing the connection handle, object descriptor, options, object handle, completion code, and reason code. If the completion code (WS-COMPCODE) is MQCC-OK, it sets WS-REQUEST-MQ-OPEN to TRUE. Otherwise, it logs an error message using 9500-LOG-ERROR, setting various error fields including ERR-LOCATION, ERR-CRITICAL, ERR-MQ, ERR-CODE-1, ERR-CODE-2, and ERR-MESSAGE. This paragraph establishes the connection to the MQ queue for subsequent message retrieval.
- Called by: 1000-INITIALIZE
- Calls: 9500-LOG-ERROR
- Lines: 255-284

### 1100-EXIT
**Purpose:** This paragraph serves as the exit point for the 1100-OPEN-REQUEST-QUEUE paragraph. It simply contains the EXIT statement, which allows control to return to the calling paragraph (1000-INITIALIZE). It does not perform any specific actions or data manipulation. It is a standard practice in COBOL to use EXIT paragraphs to define the end of a PERFORM THRU sequence.
- Called by: 1000-INITIALIZE, 1100-OPEN-REQUEST-QUEUE
- Lines: 286-287

### 1200-SCHEDULE-PSB
**Purpose:** This paragraph schedules the IMS PSB (Program Specification Block) required for accessing IMS databases. It executes the DLI SCHD command with the PSB-NAME. The DIBSTAT (DL/I Interface Block Status) is moved to IMS-RETURN-CODE. If the PSB has been scheduled more than once (PSB-SCHEDULED-MORE-THAN-ONCE), it terminates the PSB using EXEC DLI TERM and then reschedules it. If the scheduling is successful (STATUS-OK), IMS-PSB-SCHD is set to TRUE. Otherwise, it logs an error using 9500-LOG-ERROR, setting ERR-LOCATION, ERR-CRITICAL, ERR-IMS, ERR-CODE-1, and ERR-MESSAGE. This paragraph ensures that the necessary IMS resources are available for subsequent data access.
- Calls: 9500-LOG-ERROR
- Lines: 292-318

### 1200-EXIT
**Purpose:** This paragraph serves as the exit point for the 1200-SCHEDULE-PSB paragraph. It simply contains the EXIT statement, which allows control to return to the calling paragraph. It does not perform any specific actions or data manipulation. It is a standard practice in COBOL to use EXIT paragraphs to define the end of a PERFORM THRU sequence.
- Called by: 1200-SCHEDULE-PSB
- Lines: 319-321

### 2000-MAIN-PROCESS
**Purpose:** This paragraph is the main processing loop of the COPAUA0C program. It repeatedly processes authorization requests until either no more messages are available (NO-MORE-MSG-AVAILABLE) or a loop-end condition is met (WS-LOOP-END). Inside the loop, it first calls 2100-EXTRACT-REQUEST-MSG to extract the request message. Then, it calls 5000-PROCESS-AUTH to process the authorization request. After processing, it increments the message processed counter (WS-MSG-PROCESSED). It then issues a CICS SYNCPOINT to commit the changes. IMS-PSB-NOT-SCHD is set to TRUE. If the number of messages processed exceeds the limit (WS-MSG-PROCESSED > WS-REQSTS-PROCESS-LIMIT), it sets WS-LOOP-END to TRUE to terminate the loop. Otherwise, it calls 3100-READ-REQUEST-MQ to read the next message from the queue. This paragraph drives the core authorization processing logic by iterating through the messages in the queue.
- Called by: MAIN-PARA
- Calls: 2100-EXTRACT-REQUEST-MSG, 5000-PROCESS-AUTH, 3100-READ-REQUEST-MQ
- Lines: 323-345

### 2000-EXIT
**Purpose:** This paragraph serves as the exit point for the 2000-MAIN-PROCESS paragraph. It simply contains the EXIT statement, which allows control to return to the calling paragraph (MAIN-PARA). It does not perform any specific actions or data manipulation. It is a standard practice in COBOL to use EXIT paragraphs to define the end of a PERFORM THRU sequence.
- Called by: 2000-MAIN-PROCESS
- Lines: 347-348

### 2100-EXTRACT-REQUEST-MSG
**Purpose:** This paragraph extracts the authorization request message from the MQ buffer (W01-GET-BUFFER) by unstringing it based on the comma delimiter. The extracted values are moved into the corresponding fields such as PA-RQ-AUTH-DATE, PA-RQ-AUTH-TIME, PA-RQ-CARD-NUM, etc. It then converts the alphanumeric transaction amount (WS-TRANSACTION-AMT-AN) to a numeric value (PA-RQ-TRANSACTION-AMT) using the NUMVAL function and moves it to WS-TRANSACTION-AMT. The input is the W01-GET-BUFFER, and the outputs are the individual fields populated from the buffer and WS-TRANSACTION-AMT. No error handling is present in this paragraph. It does not call any other paragraphs or programs.
- Lines: 351-380

### 2100-EXIT
**Purpose:** This paragraph serves as the exit point for the 2100-EXTRACT-REQUEST-MSG paragraph. It simply contains the EXIT statement, which returns control to the calling paragraph. It does not perform any processing, read or write any data, or call any other paragraphs or programs. It is a standard COBOL construct for exiting a paragraph.
- Lines: 382-383

### 3100-READ-REQUEST-MQ
**Purpose:** This paragraph reads a message from the MQ queue. It sets the MQGMO options for no sync point, wait, convert, and fail if quiescing. It moves the wait interval to MQGMO-WAITINTERVAL. It initializes the MQMD message ID and correlation ID to MQMI-NONE and MQCI-NONE respectively, and sets the message format to MQFMT-STRING. It then calls the MQGET API to retrieve the message from the queue. If the MQGET call is successful (WS-COMPCODE = MQCC-OK), it saves the correlation ID and reply-to queue name. If the call fails and the reason code is MQRC-NO-MSG-AVAILABLE, it sets the NO-MORE-MSG-AVAILABLE flag to TRUE. Otherwise, it logs an error message. The input is the MQ queue, and the output is the message in W01-GET-BUFFER. It calls the 9500-LOG-ERROR paragraph if an error occurs.
- Calls: 9500-LOG-ERROR
- Lines: 386-432

### 3100-EXIT
**Purpose:** This paragraph serves as the exit point for the 3100-READ-REQUEST-MQ paragraph. It simply contains the EXIT statement, which returns control to the calling paragraph. It does not perform any processing, read or write any data, or call any other paragraphs or programs. It is a standard COBOL construct for exiting a paragraph.
- Lines: 434-435

### 5000-PROCESS-AUTH
**Purpose:** This paragraph is the main processing logic for authorization. It first sets APPROVE-AUTH to TRUE. Then, it calls 1200-SCHEDULE-PSB. It sets CARD-FOUND-XREF and FOUND-ACCT-IN-MSTR to TRUE. It calls 5100-READ-XREF-RECORD to read the card cross-reference record. If the card is found in the cross-reference file (CARD-FOUND-XREF), it calls 5200-READ-ACCT-RECORD, 5300-READ-CUST-RECORD, 5500-READ-AUTH-SUMMRY, and 5600-READ-PROFILE-DATA. It then calls 6000-MAKE-DECISION to determine the authorization decision and 7100-SEND-RESPONSE to send the response. Finally, if the card is found in the cross-reference file, it calls 8000-WRITE-AUTH-TO-DB to write the authorization to the database. The inputs are the card number and account ID, and the outputs are the authorization decision and the updated database. This paragraph orchestrates the entire authorization process.
- Calls: 1200-SCHEDULE-PSB, 5100-READ-XREF-RECORD, 5200-READ-ACCT-RECORD, 5300-READ-CUST-RECORD, 5500-READ-AUTH-SUMMRY, 5600-READ-PROFILE-DATA, 6000-MAKE-DECISION, 7100-SEND-RESPONSE, 8000-WRITE-AUTH-TO-DB
- Lines: 438-466

### 5000-EXIT
**Purpose:** This paragraph serves as the exit point for the 5000-PROCESS-AUTH paragraph. It simply contains the EXIT statement, which returns control to the calling paragraph. It does not perform any processing, read or write any data, or call any other paragraphs or programs. It is a standard COBOL construct for exiting a paragraph.
- Lines: 468-469

### 5100-READ-XREF-RECORD
**Purpose:** This paragraph reads the card cross-reference record from the WS-CCXREF-FILE using the card number (PA-RQ-CARD-NUM) as the key. It moves the card number to XREF-CARD-NUM and then executes a CICS READ command to retrieve the CARD-XREF-RECORD. It evaluates the CICS response code (WS-RESP-CD). If the response is NORMAL, it sets CARD-FOUND-XREF to TRUE. If the response is NOTFND, it sets CARD-NFOUND-XREF and NFOUND-ACCT-IN-MSTR to TRUE and logs a warning message. If the response is OTHER, it logs a critical error message. The input is the card number, and the output is the CARD-XREF-RECORD. It calls the 9500-LOG-ERROR paragraph if an error occurs.
- Calls: 9500-LOG-ERROR
- Lines: 472-514

### 5100-EXIT
**Purpose:** This paragraph serves as the exit point for the 5100-READ-XREF-RECORD paragraph. It simply contains the EXIT statement, which returns control to the calling paragraph. It does not perform any processing, read or write any data, or call any other paragraphs or programs. It is a standard COBOL construct for exiting a paragraph.
- Lines: 516-517

### 5200-READ-ACCT-RECORD
**Purpose:** This paragraph reads the account record from the WS-ACCTFILENAME using the account ID (XREF-ACCT-ID) obtained from the cross-reference record. It moves the account ID to WS-CARD-RID-ACCT-ID and then executes a CICS READ command to retrieve the ACCOUNT-RECORD. It evaluates the CICS response code (WS-RESP-CD). If the response is NORMAL, it sets FOUND-ACCT-IN-MSTR to TRUE. If the response is NOTFND, it sets NFOUND-ACCT-IN-MSTR to TRUE and logs a warning message. If the response is OTHER, it logs a critical error message. The input is the account ID, and the output is the ACCOUNT-RECORD. It calls the 9500-LOG-ERROR paragraph if an error occurs.
- Calls: 9500-LOG-ERROR
- Lines: 520-562

### 5200-EXIT
**Purpose:** This paragraph serves as the exit point for the 5200-READ-ACCT-RECORD paragraph. It simply contains the EXIT statement, which returns control to the calling paragraph. It does not perform any processing, read or write any data, or call any other paragraphs or programs. It is a standard COBOL construct for exiting a paragraph.
- Lines: 564-565

### 5300-READ-CUST-RECORD
**Purpose:** This paragraph reads a customer record from the VSAM file specified by WS-CUSTFILENAME using the customer ID (XREF-CUST-ID) as the key. It moves the XREF-CUST-ID to WS-CARD-RID-CUST-ID before performing the READ. The CICS READ command retrieves the CUSTOMER-RECORD. After the READ, it evaluates the WS-RESP-CD to determine if the record was found. If the record is found (DFHRESP(NORMAL)), it sets FOUND-CUST-IN-MSTR to TRUE. If the record is not found (DFHRESP(NOTFND)), it sets NFOUND-CUST-IN-MSTR to TRUE, logs an error message with location 'A003', and performs 9500-LOG-ERROR. If any other error occurs during the READ, it logs a critical CICS error with location 'C003', including the RESP and RESP2 codes, and performs 9500-LOG-ERROR.
- Calls: 9500-LOG-ERROR
- Lines: 568-610

### 5300-EXIT
**Purpose:** This paragraph is a standard EXIT paragraph. It serves as the exit point for the 5300-READ-CUST-RECORD paragraph, ensuring a clean and controlled return to the calling paragraph. It does not perform any specific logic or data manipulation. The EXIT statement simply returns control to the calling paragraph.
- Called by: 5300-READ-CUST-RECORD
- Lines: 612-613

### 5500-READ-AUTH-SUMMRY
**Purpose:** This paragraph reads the pending authorization summary from an IMS database. It moves the account ID (XREF-ACCT-ID) to PA-ACCT-ID and then executes a DLI GU (Get Unique) command using the PAUTSUM0 segment. The retrieved data is placed into PENDING-AUTH-SUMMARY. The IMS return code (DIBSTAT) is moved to IMS-RETURN-CODE. Based on the IMS return code, it sets flags: FOUND-PAUT-SMRY-SEG to TRUE if the segment is found (STATUS-OK), NFOUND-PAUT-SMRY-SEG to TRUE if the segment is not found (SEGMENT-NOT-FOUND). If any other error occurs, it logs a critical IMS error with location 'I002', including the IMS return code, and performs 9500-LOG-ERROR.
- Calls: 9500-LOG-ERROR
- Lines: 616-641

### 5500-EXIT
**Purpose:** This paragraph is a standard EXIT paragraph. It serves as the exit point for the 5500-READ-AUTH-SUMMRY paragraph, providing a clean return to the calling paragraph. It contains only the EXIT statement and performs no other actions.
- Called by: 5500-READ-AUTH-SUMMRY
- Lines: 643-644

### 5600-READ-PROFILE-DATA
**Purpose:** This paragraph currently contains only a CONTINUE statement, indicating that it does not perform any specific action. It might be a placeholder for future functionality related to reading profile data. As it stands, it effectively does nothing.
- Lines: 647-651

### 5600-EXIT
**Purpose:** This paragraph is a standard EXIT paragraph. It serves as the exit point for the 5600-READ-PROFILE-DATA paragraph. It contains only the EXIT statement and ensures a clean return to the calling paragraph.
- Called by: 5600-READ-PROFILE-DATA
- Lines: 653-654

### 6000-MAKE-DECISION
**Purpose:** This paragraph determines whether to approve or decline an authorization request based on available credit. It first moves request data (card number, transaction ID, auth time) to response fields. If a pending authorization summary is found (FOUND-PAUT-SMRY-SEG is true), it calculates available credit (WS-AVAILABLE-AMT) using the credit limit and balance from the summary. Otherwise, if an account is found (FOUND-ACCT-IN-MSTR is true), it uses the account's credit limit and current balance. If the transaction amount (WS-TRANSACTION-AMT) exceeds the available credit, it sets DECLINE-AUTH and INSUFFICIENT-FUND flags to TRUE. If DECLINE-AUTH is TRUE, it sets the authorization response code (PA-RL-AUTH-RESP-CODE) to '05' and the approved amount to 0. Otherwise, it sets the response code to '00' and the approved amount to the transaction amount. Finally, it sets a reason code (PA-RL-AUTH-RESP-REASON) based on various flags indicating the reason for decline (e.g., card not found, insufficient funds). The paragraph then formats the response data into W02-PUT-BUFFER using a STRING statement.
- Lines: 657-732

### 6000-EXIT
**Purpose:** This paragraph is a standard EXIT paragraph. It serves as the exit point for the 6000-MAKE-DECISION paragraph. It ensures a clean return to the calling paragraph and contains only the EXIT statement.
- Called by: 6000-MAKE-DECISION
- Lines: 734-735

### 7100-SEND-RESPONSE
**Purpose:** This paragraph sends the authorization response message to the reply queue using MQ. It sets the MQ object type and name in MQM-OD-REPLY, message type and correlation ID in MQM-MD-REPLY, and other MQMD fields like persistence and expiry. It sets MQPMO options for no syncpoint and default context. It then calls the MQPUT1 API to send the message in W02-PUT-BUFFER to the reply queue. If the MQPUT1 call fails (WS-COMPCODE not equal to MQCC-OK), it logs a critical MQ error with location 'M004', including the completion code and reason code, and performs 9500-LOG-ERROR.
- Calls: 9500-LOG-ERROR
- Lines: 738-780

### 7100-EXIT
**Purpose:** This paragraph is a standard EXIT paragraph. It serves as the exit point for the 7100-SEND-RESPONSE paragraph. It ensures a clean return to the calling paragraph and contains only the EXIT statement.
- Called by: 7100-SEND-RESPONSE
- Lines: 782-783

### 8000-WRITE-AUTH-TO-DB
**Purpose:** This paragraph orchestrates the update of the pending authorization summary and the insertion of authorization details into the IMS database. It first performs 8400-UPDATE-SUMMARY to update the summary information. Then, it performs 8500-INSERT-AUTH to insert the authorization details. The purpose is to ensure that both the summary and detailed information about the authorization are persisted. It consumes no direct inputs but relies on data prepared by other paragraphs. It produces no direct outputs but triggers the IMS updates in the called paragraphs. No specific business logic is implemented directly in this paragraph, it serves as a control point for the two key persistence operations. No error handling is present in this paragraph. It calls 8400-UPDATE-SUMMARY and 8500-INSERT-AUTH.
- Calls: 8400-UPDATE-SUMMARY, 8500-INSERT-AUTH
- Lines: 786-792

### 8000-EXIT
**Purpose:** This paragraph provides a standard exit point for the 8000-WRITE-AUTH-TO-DB paragraph. It ensures a consistent return from the paragraph. It does not consume any inputs or produce any outputs. It does not implement any business logic or error handling. It does not call any other paragraphs or programs. Its sole purpose is to provide a clean exit.
- Called by: 8400-UPDATE-SUMMARY, 8500-INSERT-AUTH
- Lines: 794-795

### 8400-UPDATE-SUMMARY
**Purpose:** This paragraph updates the pending authorization summary in the IMS database. It first checks if a summary segment already exists. If not, it initializes a new segment and populates it with account and customer IDs. It then updates the credit and cash limits. Based on whether the authorization was approved or declined, it increments the corresponding counters and amounts in the summary. Finally, it either inserts or replaces the summary segment in the IMS database. The paragraph consumes account information (ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, XREF-ACCT-ID, XREF-CUST-ID) and authorization response information (AUTH-RESP-APPROVED, WS-APPROVED-AMT, PA-TRANSACTION-AMT). It produces an updated PENDING-AUTH-SUMMARY segment in the IMS database. The business logic involves checking if the summary segment exists and updating counters based on the authorization response. Error handling is performed by checking the IMS return code and logging an error if the update fails. It calls 9500-LOG-ERROR if an IMS error occurs.
- Called by: 8000-WRITE-AUTH-TO-DB
- Calls: 9500-LOG-ERROR
- Lines: 798-848

### 8400-EXIT
**Purpose:** This paragraph provides a standard exit point for the 8400-UPDATE-SUMMARY paragraph. It ensures a consistent return from the paragraph. It does not consume any inputs or produce any outputs. It does not implement any business logic or error handling. It does not call any other paragraphs or programs. Its sole purpose is to provide a clean exit.
- Called by: 8000-WRITE-AUTH-TO-DB
- Lines: 850-851

### 8500-INSERT-AUTH
**Purpose:** This paragraph inserts authorization details into the IMS database. It first retrieves the current date and time using CICS services and formats them. It then calculates authorization dates and times by subtracting the current date and time from constants. It moves data from the request and response into the PENDING-AUTH-DETAILS segment. It sets flags based on whether the authorization was approved or declined. Finally, it inserts the details segment into the IMS database. The paragraph consumes authorization request and response data (PA-RQ-*, PA-RL-*) and account information (XREF-ACCT-ID). It produces a new PENDING-AUTH-DETAILS segment in the IMS database. The business logic involves calculating authorization dates and times and setting flags based on the authorization response. Error handling is performed by checking the IMS return code and logging an error if the insert fails. It calls 9500-LOG-ERROR if an IMS error occurs.
- Called by: 8000-WRITE-AUTH-TO-DB
- Calls: 9500-LOG-ERROR
- Lines: 854-933

### 8500-EXIT
**Purpose:** This paragraph provides a standard exit point for the 8500-INSERT-AUTH paragraph. It ensures a consistent return from the paragraph. It does not consume any inputs or produce any outputs. It does not implement any business logic or error handling. It does not call any other paragraphs or programs. Its sole purpose is to provide a clean exit.
- Called by: 8000-WRITE-AUTH-TO-DB
- Lines: 935-936

### 9000-TERMINATE
**Purpose:** This paragraph terminates the IMS PSB and closes the request queue. It first checks if the IMS PSB is scheduled. If so, it terminates the PSB. Then, it performs 9100-CLOSE-REQUEST-QUEUE to close the request queue. The purpose is to ensure that all resources are released before the program ends. It consumes IMS-PSB-SCHD to determine if the PSB is scheduled. It produces no direct outputs but triggers the termination of the PSB and the closing of the request queue. No specific business logic is implemented directly in this paragraph, it serves as a control point for the termination operations. No error handling is present in this paragraph. It calls 9100-CLOSE-REQUEST-QUEUE.
- Calls: 9100-CLOSE-REQUEST-QUEUE
- Lines: 940-948

### 9000-EXIT
**Purpose:** This paragraph provides a standard exit point for the 9000-TERMINATE paragraph. It ensures a consistent return from the paragraph. It does not consume any inputs or produce any outputs. It does not implement any business logic or error handling. It does not call any other paragraphs or programs. Its sole purpose is to provide a clean exit.
- Called by: 9000-TERMINATE
- Lines: 950-951

### 9100-CLOSE-REQUEST-QUEUE
**Purpose:** This paragraph closes the request queue if it is open. It checks if the request queue is open by examining WS-REQUEST-MQ-OPEN. If it is open, it calls MQCLOSE to close the queue. It then checks the completion code from MQCLOSE. If the close was successful, it sets WS-REQUEST-MQ-CLSE to TRUE. Otherwise, it logs an error. The paragraph consumes WS-REQUEST-MQ-OPEN, W01-HCONN-REQUEST, W01-HOBJ-REQUEST, MQCO-NONE, WS-COMPCODE, and WS-REASON. It produces no direct outputs but may log an error if the close fails. The business logic involves checking if the queue is open and handling the completion code from MQCLOSE. Error handling is performed by checking the completion code and logging an error if the close fails. It calls MQCLOSE and 9500-LOG-ERROR.
- Called by: 9000-TERMINATE
- Calls: MQCLOSE, 9500-LOG-ERROR
- Lines: 953-977

### 9100-EXIT
**Purpose:** This paragraph provides a standard exit point for the 9100-CLOSE-REQUEST-QUEUE paragraph. It ensures a consistent return from the paragraph. It does not consume any inputs or produce any outputs. It does not implement any business logic or error handling. It does not call any other paragraphs or programs. Its sole purpose is to provide a clean exit.
- Called by: 9000-TERMINATE
- Lines: 979-980

### 9500-LOG-ERROR
**Purpose:** This paragraph logs error information to the CICS transient data queue 'CSSL'. It first retrieves the current date and time using CICS ASKTIME and FORMATTIME commands, storing them in WS-CUR-DATE-X6 and WS-CUR-TIME-X6 respectively. It then moves the transaction ID (WS-CICS-TRANID) and program ID (WS-PGM-AUTH) into the ERROR-LOG-RECORD. The formatted error record is then written to the 'CSSL' queue. Finally, it checks if the error is critical (ERR-CRITICAL); if so, it performs the 9990-END-ROUTINE to terminate the program.
- Calls: 9990-END-ROUTINE
- Lines: 2-30

### 9500-EXIT
**Purpose:** This paragraph provides a standard exit point from the 9500-LOG-ERROR paragraph. It consists solely of the EXIT statement, which returns control to the calling paragraph.
- Called by: 9500-LOG-ERROR
- Lines: 33-34

### 9990-END-ROUTINE
**Purpose:** This paragraph performs the necessary steps to terminate the CICS transaction. It first calls the 9000-TERMINATE paragraph, presumably to perform application-specific cleanup tasks such as closing files or releasing resources. After the termination routine completes, it executes a CICS RETURN command to return control to CICS.
- Called by: 9500-LOG-ERROR
- Calls: 9000-TERMINATE
- Lines: 37-44

### 9990-EXIT
**Purpose:** This paragraph provides a standard exit point from the 9990-END-ROUTINE paragraph. It consists solely of the EXIT statement, which returns control to the calling paragraph.
- Called by: 9990-END-ROUTINE
- Lines: 47-48

### COPAUA0C
**Purpose:** This is the main paragraph, and in this case, the only paragraph defined in the provided code snippet. It declares the program ID as COPAUA0C. Without further code, its purpose cannot be determined beyond this declaration. It does not consume any specific inputs or produce any outputs based on the snippet. No business logic, error handling, or calls to other paragraphs or programs are present in this snippet. The program's functionality is entirely UNKNOWN without additional code.
- Lines: 23-23

## Error Handling

- **WS-COMPCODE NOT = MQCC-OK:** MOVE 'M001' TO ERR-LOCATION, SET ERR-CRITICAL, ERR-MQ TO TRUE, MOVE WS-COMPCODE TO ERR-CODE-1, MOVE WS-REASON TO ERR-CODE-2, MOVE 'REQ MQ OPEN ERROR' TO ERR-MESSAGE, PERFORM 9500-LOG-ERROR
  (Lines: 54, 66)
- **STATUS-OK is false:** MOVE 'I001' TO ERR-LOCATION, SET ERR-CRITICAL, ERR-IMS TO TRUE, MOVE IMS-RETURN-CODE TO ERR-CODE-1, MOVE 'IMS SCHD FAILED' TO ERR-MESSAGE, PERFORM 9500-LOG-ERROR
  (Lines: 91, 100)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| RETRIEVE | MQTM | Retrieves data from the CICS terminal. | 18 |
| RETURN |  | Returns control to CICS. | 11 |
| SYNCPOINT |  | Commits changes. | 121 |

## Open Questions

- **What is the structure of the MQM-OD-REQUEST?**
  - Context: The code moves data into fields of MQM-OD-REQUEST, but the structure is not defined in the provided code snippet.
  - Suggestion: Examine the copybook definitions to determine the structure of MQM-OD-REQUEST.
- **What is the purpose of 2100-EXTRACT-REQUEST-MSG and 5000-PROCESS-AUTH?**
  - Context: These paragraphs are called but not defined in the provided code snippet.
  - Suggestion: Examine the source code for these paragraphs to understand their functionality.
- **What are the conditions for NO-MORE-MSG-AVAILABLE?**
  - Context: This variable controls the main processing loop, but its definition is not provided.
  - Suggestion: Examine the source code to determine how NO-MORE-MSG-AVAILABLE is set.

## Resolved Questions

- **Q:** What is the purpose of WS-TRIGGER-DATA?
  **A:** The code shows that `WS-TRIGGER-DATA` is a 64-byte field that receives the value of `MQTM-TRIGGERDATA` after a successful RECEIVE command from the MQ queue. `MQTM-TRIGGERDATA` likely contains data that triggered the message to be placed on the queue.

---
*Generated by War Rig WAR_RIG*