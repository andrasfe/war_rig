# COPAUA0C

**File**: `cbl/COPAUA0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-02-10 17:19:28.909600

## Purpose

This CICS COBOL program, COPAUA0C, is a card authorization decision program that processes authorization requests, validates card and customer data, makes an authorization decision, and updates the database with the authorization results. It retrieves requests from an MQ queue, processes them, and sends a response.

**Business Context**: Card authorization process within a financial transaction system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| MQ Request Queue | IOType.CICS_QUEUE | Authorization request messages from MQ Series. |
| CARD-XREF-RECORD | IOType.FILE_VSAM | Card cross-reference data containing account and customer IDs. |
| ACCOUNT-RECORD | IOType.FILE_VSAM | Account details for the card. |
| CUSTOMER-RECORD | IOType.FILE_VSAM | Customer information associated with the account. |
| PENDING-AUTH-SUMMARY | IOType.IMS_SEGMENT | Summary of pending authorizations for the account. |
| DFHCOMMAREA | IOType.CICS_COMMAREA | CICS Commarea for passing data between programs. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| MQ Reply Queue | IOType.CICS_QUEUE | Authorization response messages sent back to the requestor. |
| PAUTSUM0 | IOType.IMS_SEGMENT | Updated pending authorization summary in IMS database. |
| PAUTTRAN | IOType.IMS_SEGMENT | New authorization transaction record in IMS database. |

## Business Rules

- **BR001**: If the card is not found in the cross-reference file, decline the authorization.
- **BR002**: If the number of processed messages exceeds the limit, the program stops processing.

## Paragraphs/Procedures

### MAIN-PARA
This is the main control paragraph of the COPAUA0C program. It orchestrates the entire authorization process by calling other paragraphs in sequence. First, it performs 1000-INITIALIZE to retrieve the MQ trigger monitor message and open the request queue. Then, it calls 2000-MAIN-PROCESS to handle the actual authorization logic, which includes extracting the request message, processing the authorization, and sending the response. Finally, it performs 9000-TERMINATE to close any open resources and return control to CICS. The paragraph does not directly handle any business logic or data manipulation but acts as the central coordinator for the program's execution flow. It ensures that the necessary initialization, processing, and termination steps are executed in the correct order. After termination, the program returns to CICS.

### 1000-INITIALIZE
This paragraph initializes the program by retrieving the MQ trigger monitor message, opening the request queue, and reading the first request message. It starts by retrieving the MQ trigger monitor message using EXEC CICS RETRIEVE, storing the queue name and trigger data in working storage (WS-REQUEST-QNAME, WS-TRIGGER-DATA). If the retrieval is successful (EIBRESP = DFHRESP(NORMAL)), it moves the retrieved data to the corresponding working storage variables. It then sets the wait interval for MQ operations to 5000. Next, it performs 1100-OPEN-REQUEST-QUEUE to open the MQ request queue for processing. Finally, it performs 3100-READ-REQUEST-MQ to read the first authorization request message from the queue. This paragraph sets up the program for subsequent processing by ensuring that the necessary resources are available and the initial data is loaded. No error handling is explicitly performed in this paragraph, but the EXEC CICS RETRIEVE command includes the NOHANDLE option, which suppresses CICS abends.

### 2000-MAIN-PROCESS
This paragraph is the core processing loop of the program, handling the authorization requests until a termination condition is met. It repeatedly performs a series of operations for each message available in the request queue. The loop continues until either NO-MORE-MSG-AVAILABLE is true or WS-LOOP-END is true. Inside the loop, it first performs 2100-EXTRACT-REQUEST-MSG to extract the request message from the buffer. Then, it performs 5000-PROCESS-AUTH to process the authorization request and make a decision. After processing, it increments the WS-MSG-PROCESSED counter and issues a CICS SYNCPOINT to commit the changes. IMS PSB is set to NOT SCHD. It then checks if the number of processed messages (WS-MSG-PROCESSED) exceeds the limit (WS-REQSTS-PROCESS-LIMIT). If the limit is reached, it sets WS-LOOP-END to TRUE, terminating the loop. Otherwise, it performs 3100-READ-REQUEST-MQ to read the next request message from the queue. This paragraph manages the overall flow of authorization processing, ensuring that each request is handled and the system remains synchronized. Error handling is implicit through the PERFORM THRU constructs, which would propagate control to error handling routines if any of the called paragraphs encounter an error.

### 5000-PROCESS-AUTH
This paragraph orchestrates the authorization processing for a single card transaction. It begins by setting APPROVE-AUTH to TRUE, assuming the transaction will be approved unless a reason to decline is found. It then performs 1200-SCHEDULE-PSB to schedule the PSB for IMS database access. It sets CARD-FOUND-XREF and FOUND-ACCT-IN-MSTR to TRUE. Next, if CARD-FOUND-XREF is true, it performs 5100-READ-XREF-RECORD to read the card cross-reference record, 5200-READ-ACCT-RECORD to read the account record, 5300-READ-CUST-RECORD to read the customer record, 5500-READ-AUTH-SUMMRY to read the authorization summary, and 5600-READ-PROFILE-DATA to read the profile data. It then performs 6000-MAKE-DECISION to make the authorization decision and 7100-SEND-RESPONSE to send the response. Finally, if CARD-FOUND-XREF is true, it performs 8000-WRITE-AUTH-TO-DB to write the authorization details to the database. This paragraph manages the sequence of steps required to authorize a transaction, including data retrieval, decision making, and database updates. Error handling is performed within the subordinate paragraphs, and the overall flow is controlled by conditional PERFORM statements based on the CARD-FOUND-XREF flag.

### 5100-READ-XREF-RECORD
This paragraph reads the card cross-reference record from the VSAM file WS-CCXREF-FILE. It moves the card number from the request (PA-RQ-CARD-NUM) to the key field (XREF-CARD-NUM). It then executes a CICS READ command to retrieve the CARD-XREF-RECORD from the VSAM file using the key. The RESP and RESP2 options capture the CICS response codes in WS-RESP-CD and WS-REAS-CD, respectively. The paragraph then evaluates the response code. If the read is successful (DFHRESP(NORMAL)), it sets CARD-FOUND-XREF to TRUE. If the record is not found (DFHRESP(NOTFND)), it sets CARD-NFOUND-XREF and NFOUND-ACCT-IN-MSTR to TRUE, logs an error message, and sets error flags. If any other error occurs, it logs a critical error message with the CICS response codes. This paragraph is responsible for retrieving the cross-reference record, which links the card number to the account and customer IDs. Error handling includes logging warning and critical errors based on the CICS response codes. The paragraph does not call any other paragraphs.

### 5200-READ-ACCT-RECORD
This paragraph reads the account record from the VSAM file WS-ACCTFILENAME. It moves the account ID from the cross-reference record (XREF-ACCT-ID) to the key field (WS-CARD-RID-ACCT-ID). It then executes a CICS READ command to retrieve the ACCOUNT-RECORD from the VSAM file using the key. The RESP and RESP2 options capture the CICS response codes in WS-RESP-CD and WS-REAS-CD, respectively. The paragraph then evaluates the response code. If the read is successful (DFHRESP(NORMAL)), it sets FOUND-ACCT-IN-MSTR to TRUE. If the record is not found (DFHRESP(NOTFND)), it sets NFOUND-ACCT-IN-MSTR to TRUE and logs an error message. If any other error occurs, it logs a critical error message with the CICS response codes. This paragraph is responsible for retrieving the account record, which contains the account details for the card. Error handling includes logging warning and critical errors based on the CICS response codes. The paragraph does not call any other paragraphs.

### 5300-READ-CUST-RECORD
This paragraph reads the customer record from the VSAM file WS-CUSTFILENAME. It moves the customer ID from the cross-reference record (XREF-CUST-ID) to the key field (WS-CARD-RID-CUST-ID). It then executes a CICS READ command to retrieve the CUSTOMER-RECORD from the VSAM file using the key. The RESP and RESP2 options capture the CICS response codes in WS-RESP-CD and WS-REAS-CD, respectively. The paragraph then evaluates the response code. If the read is successful (DFHRESP(NORMAL)), it sets FOUND-CUST-IN-MSTR to TRUE. If the record is not found (DFHRESP(NOTFND)), it sets NFOUND-CUST-IN-MSTR to TRUE and logs an error message. If any other error occurs, it logs a critical error message with the CICS response codes. This paragraph is responsible for retrieving the customer record, which contains the customer details associated with the account. Error handling includes logging warning and critical errors based on the CICS response codes. The paragraph does not call any other paragraphs.

### 5500-READ-AUTH-SUMMRY
This paragraph reads the pending authorization summary segment (PAUTSUM0) from the IMS database. It moves the account ID from the cross-reference record (XREF-ACCT-ID) to the PA-ACCT-ID field. It then executes a DLI GU (Get Unique) command to retrieve the PAUTSUM0 segment using the PAUT-PCB-NUM PCB. The WHERE clause specifies that the ACCNTID field in the segment must match PA-ACCT-ID. The DIBSTAT field is moved to IMS-RETURN-CODE to check the status of the IMS call. The paragraph then evaluates the IMS-RETURN-CODE. If the status is OK, it sets FOUND-PAUT-SMRY-SEG to TRUE. If the segment is not found, it sets NFOUND-PAUT-SMRY-SEG to TRUE. If any other error occurs, it logs a critical error message with the IMS return code. This paragraph is responsible for retrieving the pending authorization summary, which is used to make the authorization decision. Error handling includes logging critical errors based on the IMS return code. The paragraph does not call any other paragraphs.

### 5600-READ-PROFILE-DATA
This paragraph is a placeholder and currently contains only a CONTINUE statement. It does not perform any actions. It serves as a stub for future implementation where profile data might be read. Currently, it does not read any data, perform any logic, or call any other paragraphs. It simply allows the program to proceed to the next step in the authorization process without performing any profile data retrieval. This paragraph exists to maintain a consistent program structure and allow for future expansion without requiring significant code changes to the calling paragraph.

### 7100-SEND-RESPONSE
This paragraph sends the authorization response to the MQ reply queue. It moves the card number and transaction ID from the request to the response fields (PA-RQ-CARD-NUM to PA-RL-CARD-NUM, PA-RQ-TRANSACTION-ID to PA-RL-TRANSACTION-ID). It also moves the authorization time to the auth ID code. It then moves the authorization response code (PA-RL-AUTH-RESP-CODE) to the response message buffer (W02-PUT-BUFFER). The paragraph then calculates the length of the response message and moves it to W02-DATALEN. It then executes a series of MQ API calls to put the message on the reply queue. If any MQ API call fails, it logs an error message. This paragraph is responsible for sending the authorization decision back to the requestor. Error handling includes logging errors if any of the MQ API calls fail. The paragraph calls 9500-LOG-ERROR to log error messages.

### 8000-WRITE-AUTH-TO-DB
This paragraph coordinates the writing of authorization information to the IMS database. It performs 8400-UPDATE-SUMMARY to update the pending authorization summary segment and 8500-INSERT-AUTH to insert a new authorization transaction segment. This paragraph serves as a high-level control point for database updates related to the authorization process. It ensures that both the summary and transaction records are updated appropriately. The paragraph does not directly handle any data manipulation or database calls but delegates these tasks to the subordinate paragraphs. The success of the database updates is dependent on the proper execution of the called paragraphs. The paragraph does not perform any explicit error handling itself, relying on the error handling within the called paragraphs.

### COPAUA0C
This paragraph is the program identifier. It does not contain any executable code. It serves as the entry point for the program but immediately passes control to other paragraphs for processing. It is the starting point for the program's execution flow, but its primary function is to define the program's name and initiate the subsequent processing steps. It does not directly interact with any data or perform any business logic. The program's execution begins here, but the actual work is delegated to other paragraphs. This paragraph does not handle any error conditions or make any decisions. It simply marks the beginning of the program's execution.

### 1000-EXIT
This paragraph simply marks the exit point for the 1000-INITIALIZE paragraph. It contains only the EXIT statement and does not perform any processing or data manipulation. It serves as a standard way to define the end of a PERFORM THRU sequence. This paragraph does not consume any input or produce any output. It does not implement any business logic or error handling. It is a placeholder for the end of the initialization process.

### 1100-OPEN-REQUEST-QUEUE
This paragraph opens the request message queue. It moves MQOT-Q to MQOD-OBJECTTYPE and WS-REQUEST-QNAME to MQOD-OBJECTNAME to define the queue object (lines 36-37). It then computes WS-OPTIONS as MQOO-INPUT-SHARED (line 39). It calls MQOPEN to open the queue using the specified parameters. If MQOPEN is successful (WS-COMPCODE = MQCC-OK), it sets WS-REQUEST-MQ-OPEN to TRUE (lines 49-50). Otherwise, it logs an error using 9500-LOG-ERROR (lines 52-61). The paragraph consumes WS-REQUEST-QNAME as input and sets WS-REQUEST-MQ-OPEN based on the MQOPEN result. It produces no direct output but opens the message queue. It implements error handling for MQOPEN failures. It calls MQOPEN to open the queue and 9500-LOG-ERROR to handle errors.

### 1100-EXIT
This paragraph simply marks the exit point for the 1100-OPEN-REQUEST-QUEUE paragraph. It contains only the EXIT statement and does not perform any processing or data manipulation. It serves as a standard way to define the end of a PERFORM THRU sequence. This paragraph does not consume any input or produce any output. It does not implement any business logic or error handling. It is a placeholder for the end of the queue opening process.

### 1200-SCHEDULE-PSB
This paragraph schedules a PSB (Program Specification Block) in IMS. It uses the EXEC DLI SCHD command to schedule the PSB specified by PSB-NAME (lines 69-72). It then moves DIBSTAT to IMS-RETURN-CODE to capture the IMS return code. If the PSB was scheduled more than once, it terminates the PSB and schedules it again (lines 74-83). If the status is OK, it sets IMS-PSB-SCHD to TRUE (lines 84-85). Otherwise, it logs an error using 9500-LOG-ERROR (lines 87-93). The paragraph consumes PSB-NAME as input and sets IMS-PSB-SCHD based on the IMS scheduling result. It produces no direct output but schedules the PSB. It implements error handling for IMS scheduling failures. It calls 9500-LOG-ERROR to handle errors.

### 1200-EXIT
This paragraph simply marks the exit point for the 1200-SCHEDULE-PSB paragraph. It contains only the EXIT statement and does not perform any processing or data manipulation. It serves as a standard way to define the end of a PERFORM THRU sequence. This paragraph does not consume any input or produce any output. It does not implement any business logic or error handling. It is a placeholder for the end of the PSB scheduling process.

### 2000-EXIT
This paragraph simply marks the exit point for the 2000-MAIN-PROCESS paragraph. It contains only the EXIT statement and does not perform any processing or data manipulation. It serves as a standard way to define the end of a PERFORM THRU sequence. This paragraph does not consume any input or produce any output. It does not implement any business logic or error handling. It is a placeholder for the end of the main processing loop.

### 2100-EXTRACT-REQUEST-MSG
This paragraph extracts data from the authorization request message (W01-GET-BUFFER) received from the MQ queue. It uses the UNSTRING statement to parse the comma-delimited message into individual fields, such as authorization date, time, card number, and transaction amount. The extracted data is then moved into corresponding fields in the PA-RQ-* and WS-* variables. The alphanumeric transaction amount (WS-TRANSACTION-AMT-AN) is converted to a numeric value using the FUNCTION NUMVAL and stored in PA-RQ-TRANSACTION-AMT and WS-TRANSACTION-AMT. This paragraph prepares the data for subsequent processing and validation. No error handling is performed within this paragraph. It is called by an upstream paragraph (not shown in the provided code) and calls no other paragraphs.

### 2100-EXIT
This paragraph serves as the exit point for the 2100-EXTRACT-REQUEST-MSG paragraph. It contains a simple EXIT statement, which returns control to the calling paragraph. It does not perform any specific actions or data manipulation. It is a standard practice in COBOL to have an explicit exit paragraph for each processing paragraph to ensure proper program flow and maintainability. This paragraph consumes no inputs and produces no outputs. It is called by 2100-EXTRACT-REQUEST-MSG and calls no other paragraphs.

### 3100-READ-REQUEST-MQ
This paragraph reads the authorization request message from the MQ queue. It sets the MQGMO options for the MQGET call, including no syncpoint, waiting for a message, conversion, and failing if quiescing. It also sets the wait interval and message format. The MQGET call retrieves the message from the queue, placing it into W01-GET-BUFFER. If the MQGET call is successful (WS-COMPCODE = MQCC-OK), the correlation ID and reply queue name are saved. If the call fails, the paragraph checks if the reason is MQRC-NO-MSG-AVAILABLE, setting a flag if no message is available. Otherwise, it logs an error message using 9500-LOG-ERROR, including the completion code, reason code, and relevant error information. The paragraph consumes the MQ queue identified by W01-HCONN-REQUEST and W01-HOBJ-REQUEST and produces the message in W01-GET-BUFFER. It is called by an upstream paragraph (not shown) and calls MQGET and 9500-LOG-ERROR.

### 3100-EXIT
This paragraph serves as the exit point for the 3100-READ-REQUEST-MQ paragraph. It contains a simple EXIT statement, which returns control to the calling paragraph. It does not perform any specific actions or data manipulation. It is a standard practice in COBOL to have an explicit exit paragraph for each processing paragraph to ensure proper program flow and maintainability. This paragraph consumes no inputs and produces no outputs. It is called by 3100-READ-REQUEST-MQ and calls no other paragraphs.

### 5000-EXIT
This paragraph serves as the exit point for the 5000-PROCESS-AUTH paragraph. It contains a simple EXIT statement, which returns control to the calling paragraph. It does not perform any specific actions or data manipulation. It is a standard practice in COBOL to have an explicit exit paragraph for each processing paragraph to ensure proper program flow and maintainability. This paragraph consumes no inputs and produces no outputs. It is called by 5000-PROCESS-AUTH and calls no other paragraphs.

### 5100-EXIT
This paragraph serves as the exit point for the 5100-READ-XREF-RECORD paragraph. It contains a simple EXIT statement, which returns control to the calling paragraph. It does not perform any specific actions or data manipulation. It is a standard practice in COBOL to have an explicit exit paragraph for each processing paragraph to ensure proper program flow and maintainability. This paragraph consumes no inputs and produces no outputs. It is called by 5100-READ-XREF-RECORD and calls no other paragraphs.

### 5200-EXIT
This paragraph serves as the exit point for the 5200-READ-ACCT-RECORD paragraph. It contains a simple EXIT statement, which returns control to the calling paragraph. It does not perform any specific actions or data manipulation. It is a standard practice in COBOL to have an explicit exit paragraph for each processing paragraph to ensure proper program flow and maintainability. This paragraph consumes no inputs and produces no outputs. It is called by 5200-READ-ACCT-RECORD and calls no other paragraphs.

### 5300-EXIT
This paragraph simply contains the EXIT statement, providing a standard exit point for the 5300-READ-CUST-RECORD paragraph. It does not perform any specific logic or data manipulation. It is called by 5300-READ-CUST-RECORD after the CICS READ operation and error handling are complete. The paragraph consumes no input and produces no output. It serves as a structured way to return control to the calling paragraph.

### 5500-EXIT
This paragraph simply contains the EXIT statement, providing a standard exit point for the 5500-READ-AUTH-SUMMRY paragraph. It does not perform any specific logic or data manipulation. It is called by 5500-READ-AUTH-SUMMRY after the IMS READ operation and error handling are complete. The paragraph consumes no input and produces no output. It serves as a structured way to return control to the calling paragraph.

### 5600-EXIT
This paragraph simply contains the EXIT statement, providing a standard exit point for the 5600-READ-PROFILE-DATA paragraph. It does not perform any specific logic or data manipulation. It is called by 5600-READ-PROFILE-DATA after the (currently empty) READ operation. The paragraph consumes no input and produces no output. It serves as a structured way to return control to the calling paragraph.

### 6000-MAKE-DECISION
This paragraph determines whether to approve or decline an authorization request. It first moves data from request fields (PA-RQ-*) to response fields (PA-RL-*). If a pending authorization summary is found (FOUND-PAUT-SMRY-SEG), it calculates the available amount (WS-AVAILABLE-AMT) using the credit limit and balance from the summary. If no summary is found but an account is found (FOUND-ACCT-IN-MSTR), it calculates the available amount using the credit limit and balance from the account record. If neither a summary nor an account is found, it declines the authorization. If the transaction amount exceeds the available amount, it declines the authorization and sets the INSUFFICIENT-FUND flag. If the authorization is declined, it sets the AUTH-RESP-DECLINED flag and moves '05' to PA-RL-AUTH-RESP-CODE. Otherwise, it sets the AUTH-RESP-APPROVED flag and moves '00' to PA-RL-AUTH-RESP-CODE and the transaction amount to PA-RL-APPROVED-AMT. Finally, it sets the reason code (PA-RL-AUTH-RESP-REASON) based on various conditions, such as card not found, insufficient funds, or fraud. The paragraph consumes data from the CICS COMMAREA, PENDING-AUTH-SUMMARY, and CUSTOMER-RECORD, and produces the authorization decision and response data in PA-RL-* fields.

### 6000-EXIT
This paragraph simply contains the EXIT statement, providing a standard exit point for the 6000-MAKE-DECISION paragraph. It does not perform any specific logic or data manipulation. It is called by 6000-MAKE-DECISION after the authorization decision logic is complete. The paragraph consumes no input and produces no output. It serves as a structured way to return control to the calling paragraph.

### 7100-EXIT
This paragraph simply contains the EXIT statement, providing a standard exit point for the 7100-SEND-RESPONSE paragraph. It does not perform any specific logic or data manipulation. It is called by 7100-SEND-RESPONSE after the MQPUT1 call and error handling are complete. The paragraph consumes no input and produces no output. It serves as a structured way to return control to the calling paragraph.

### 8000-EXIT
This paragraph serves as the exit point for the 8000-WRITE-AUTH-TO-DB paragraph. It contains a simple EXIT statement, which allows the program to return control to the calling paragraph. It does not perform any data manipulation, business logic, or error handling. It is a standard COBOL construct for defining the end of a PERFORM THRU range. This paragraph does not consume any inputs or produce any outputs. It is called by the PERFORM statement in 8000-WRITE-AUTH-TO-DB after the 8500-INSERT-AUTH paragraph has completed. It does not call any other paragraphs or programs.

### 8400-UPDATE-SUMMARY
This paragraph updates the pending authorization summary in the IMS database. It first checks if a summary segment already exists (NFOUND-PAUT-SMRY-SEG). If not, it initializes the PENDING-AUTH-SUMMARY with zeros and moves the XREF-ACCT-ID and XREF-CUST-ID to the summary record. It then moves ACCT-CREDIT-LIMIT and ACCT-CASH-CREDIT-LIMIT to the summary record. If the authorization is approved (AUTH-RESP-APPROVED), it increments the approved authorization count and amount, and updates the credit balance. Otherwise, it increments the declined authorization count and amount. Finally, it either updates an existing summary segment or inserts a new one into the IMS database using EXEC DLI REPL or ISRT commands, respectively. If the IMS operation fails, it logs an error using 9500-LOG-ERROR. Inputs include XREF-ACCT-ID, XREF-CUST-ID, ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, WS-APPROVED-AMT, and PA-TRANSACTION-AMT. Outputs include the updated PENDING-AUTH-SUMMARY segment in the IMS database. The paragraph is called by 8000-WRITE-AUTH-TO-DB.

### 8400-EXIT
This paragraph serves as the exit point for the 8400-UPDATE-SUMMARY paragraph. It contains a simple EXIT statement, which allows the program to return control to the calling paragraph. It does not perform any data manipulation, business logic, or error handling. It is a standard COBOL construct for defining the end of a PERFORM THRU range. This paragraph does not consume any inputs or produce any outputs. It is called by the PERFORM statement in 8000-WRITE-AUTH-TO-DB after the 8400-UPDATE-SUMMARY paragraph has completed. It does not call any other paragraphs or programs.

### 8500-INSERT-AUTH
This paragraph inserts authorization details into the IMS database. It first retrieves the current date and time using CICS ASKTIME and FORMATTIME commands, converting the date and time into specific formats. It then computes PA-AUTH-DATE-9C and PA-AUTH-TIME-9C by subtracting the current date and time from constant values. It moves various fields from the request (PA-RQ-*) and response (PA-RL-*) structures to the detail record. It sets PA-MATCH-PENDING or PA-MATCH-AUTH-DECLINED based on whether the authorization was approved. It then inserts the PENDING-AUTH-DETAILS segment into the IMS database using EXEC DLI ISRT. If the IMS operation fails, it logs an error using 9500-LOG-ERROR. Inputs include various PA-RQ-* and PA-RL-* fields. Outputs include the inserted PENDING-AUTH-DETAILS segment in the IMS database. The paragraph is called by 8000-WRITE-AUTH-TO-DB.

### 8500-EXIT
This paragraph serves as the exit point for the 8500-INSERT-AUTH paragraph. It contains a simple EXIT statement, which allows the program to return control to the calling paragraph. It does not perform any data manipulation, business logic, or error handling. It is a standard COBOL construct for defining the end of a PERFORM THRU range. This paragraph does not consume any inputs or produce any outputs. It is called by the PERFORM statement in 8000-WRITE-AUTH-TO-DB after the 8500-INSERT-AUTH paragraph has completed. It does not call any other paragraphs or programs.

### 9000-TERMINATE
This paragraph handles the termination of the IMS PSB and closes the request queue. It first checks if the IMS PSB is scheduled (IMS-PSB-SCHD). If so, it terminates the PSB using EXEC DLI TERM. Then, it calls 9100-CLOSE-REQUEST-QUEUE to close the request queue. There are no explicit inputs or outputs consumed or produced within this paragraph itself, as it delegates the queue closing to the called paragraph. The business logic involves checking if the IMS PSB is scheduled and terminating it if necessary. Error handling is performed by the called paragraph 9100-CLOSE-REQUEST-QUEUE. The paragraph calls 9100-CLOSE-REQUEST-QUEUE to close the request queue.

### 9000-EXIT
This paragraph serves as the exit point for the 9000-TERMINATE paragraph. It contains a simple EXIT statement, which allows the program to return control to the calling paragraph. It does not perform any data manipulation, business logic, or error handling. It is a standard COBOL construct for defining the end of a PERFORM THRU range. This paragraph does not consume any inputs or produce any outputs. It is called by the PERFORM statement in the calling paragraph after the 9000-TERMINATE paragraph has completed. It does not call any other paragraphs or programs.

### 9100-CLOSE-REQUEST-QUEUE
This paragraph closes the request message queue. It first checks if the request queue is open (WS-REQUEST-MQ-OPEN). If so, it calls the MQCLOSE program to close the queue, passing the connection handle (W01-HCONN-REQUEST), object handle (W01-HOBJ-REQUEST), and other MQ parameters. After the MQCLOSE call, it checks the completion code (WS-COMPCODE). If the completion code is MQCC-OK, it sets WS-REQUEST-MQ-CLSE to TRUE. Otherwise, it logs an error using 9500-LOG-ERROR. Inputs include W01-HCONN-REQUEST and W01-HOBJ-REQUEST. There are no explicit outputs. The paragraph is called by 9000-TERMINATE.

### 9100-EXIT
This paragraph serves as the exit point for the 9100-CLOSE-REQUEST-QUEUE paragraph. It contains a simple EXIT statement, which allows the program to return control to the calling paragraph. It does not perform any data manipulation, business logic, or error handling. It is a standard COBOL construct for defining the end of a PERFORM THRU range. This paragraph does not consume any inputs or produce any outputs. It is called by the PERFORM statement in 9000-TERMINATE after the 9100-CLOSE-REQUEST-QUEUE paragraph has completed. It does not call any other paragraphs or programs.

### 9500-LOG-ERROR
This paragraph logs error information to a CICS temporary data queue (TDQ) named CSSL. It first retrieves the current date and time using CICS ASKTIME and FORMATTIME commands, storing them in WS-CUR-DATE-X6 and WS-CUR-TIME-X6 respectively. It then moves the transaction ID (WS-CICS-TRANID), program name (WS-PGM-AUTH), current date, and current time into the ERROR-LOG-RECORD. The ERROR-LOG-RECORD is then written to the CSSL TDQ using the CICS WRITEQ TD command. Finally, it checks the ERR-CRITICAL flag; if set, it performs the 9990-END-ROUTINE paragraph to terminate the CICS task. This paragraph consumes WS-CICS-TRANID and WS-PGM-AUTH and produces a record in the CSSL queue.

### ~~9500-EXIT~~ (Dead Code)
*Paragraph '9500-EXIT' is never PERFORMed or referenced by any other paragraph or program*

### 9990-END-ROUTINE
This paragraph performs the necessary steps to terminate the CICS task. It first calls the 9000-TERMINATE paragraph, presumably to perform any application-specific cleanup or finalization tasks. After 9000-TERMINATE completes, it executes a CICS RETURN command to terminate the CICS task and return control to CICS. This paragraph is called by 9500-LOG-ERROR when a critical error is detected. It ensures that the application terminates gracefully after logging the error.

### ~~9990-EXIT~~ (Dead Code)
*Paragraph '9990-EXIT' is never PERFORMed or referenced by any other paragraph or program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| 9500-EXIT | paragraph | 1012 | Paragraph '9500-EXIT' is never PERFORMed or referenced by any other paragraph or program |
| 9990-EXIT | paragraph | 1024 | Paragraph '9990-EXIT' is never PERFORMed or referenced by any other paragraph or program |

## Control Flow

```mermaid
flowchart TD
    %% Title: COPAUA0C.cbl
    1000_EXIT["1000-EXIT"]
    1000_INITIALIZE["1000-INITIALIZE"]
    1100_OPEN_REQUEST_QUEUE["1100-OPEN-REQUEST-QUEUE"]
    3100_READ_REQUEST_MQ["3100-READ-REQUEST-MQ"]
    1100_EXIT["1100-EXIT"]
    9500_LOG_ERROR["9500-LOG-ERROR"]
    MQOPEN__ext(["MQOPEN"])
    1200_EXIT["1200-EXIT"]
    1200_SCHEDULE_PSB["1200-SCHEDULE-PSB"]
    2000_EXIT["2000-EXIT"]
    2000_MAIN_PROCESS["2000-MAIN-PROCESS"]
    2100_EXTRACT_REQUEST_MSG["2100-EXTRACT-REQUEST-MSG"]
    5000_PROCESS_AUTH["5000-PROCESS-AUTH"]
    2100_EXIT["2100-EXIT"]
    3100_EXIT["3100-EXIT"]
    MQGET__ext(["MQGET"])
    5000_EXIT["5000-EXIT"]
    5100_READ_XREF_RECORD["5100-READ-XREF-RECORD"]
    5200_READ_ACCT_RECORD["5200-READ-ACCT-RECORD"]
    5300_READ_CUST_RECORD["5300-READ-CUST-RECORD"]
    5500_READ_AUTH_SUMMRY["5500-READ-AUTH-SUMMRY"]
    5600_READ_PROFILE_DATA["5600-READ-PROFILE-DATA"]
    6000_MAKE_DECISION["6000-MAKE-DECISION"]
    7100_SEND_RESPONSE["7100-SEND-RESPONSE"]
    8000_WRITE_AUTH_TO_DB["8000-WRITE-AUTH-TO-DB"]
    5100_EXIT["5100-EXIT"]
    WS_CCXREF_FILE__ext[("WS-CCXREF-FILE")]
    5200_EXIT["5200-EXIT"]
    WS_ACCTFILENAME__ext[("WS-ACCTFILENAME")]
    5300_EXIT["5300-EXIT"]
    WS_CUSTFILENAME__ext[("WS-CUSTFILENAME")]
    5500_EXIT["5500-EXIT"]
    5600_EXIT["5600-EXIT"]
    6000_EXIT["6000-EXIT"]
    7100_EXIT["7100-EXIT"]
    MQPUT1__ext(["MQPUT1"])
    8000_EXIT["8000-EXIT"]
    8400_UPDATE_SUMMARY["8400-UPDATE-SUMMARY"]
    8500_INSERT_AUTH["8500-INSERT-AUTH"]
    8400_EXIT["8400-EXIT"]
    8500_EXIT["8500-EXIT"]
    9000_EXIT["9000-EXIT"]
    9000_TERMINATE["9000-TERMINATE"]
    9100_CLOSE_REQUEST_QUEUE["9100-CLOSE-REQUEST-QUEUE"]
    MQCLOSE__ext(["MQCLOSE"])
    9100_EXIT["9100-EXIT"]
    9500_EXIT["9500-EXIT"]
    9990_END_ROUTINE["9990-END-ROUTINE"]
    9990_EXIT["9990-EXIT"]
    MAIN_PARA["MAIN-PARA"]
    1000_INITIALIZE --> 1100_OPEN_REQUEST_QUEUE
    1000_INITIALIZE --> 3100_READ_REQUEST_MQ
    1100_OPEN_REQUEST_QUEUE --> 9500_LOG_ERROR
    1100_OPEN_REQUEST_QUEUE -.->|calls| MQOPEN__ext
    1200_SCHEDULE_PSB --> 9500_LOG_ERROR
    2000_MAIN_PROCESS --> 2100_EXTRACT_REQUEST_MSG
    2000_MAIN_PROCESS --> 3100_READ_REQUEST_MQ
    2000_MAIN_PROCESS --> 5000_PROCESS_AUTH
    3100_READ_REQUEST_MQ --> 9500_LOG_ERROR
    3100_READ_REQUEST_MQ -.->|calls| MQGET__ext
    5000_PROCESS_AUTH --> 1200_SCHEDULE_PSB
    5000_PROCESS_AUTH --> 5100_READ_XREF_RECORD
    5000_PROCESS_AUTH --> 5200_READ_ACCT_RECORD
    5000_PROCESS_AUTH --> 5300_READ_CUST_RECORD
    5000_PROCESS_AUTH --> 5500_READ_AUTH_SUMMRY
    5000_PROCESS_AUTH --> 5600_READ_PROFILE_DATA
    5000_PROCESS_AUTH --> 6000_MAKE_DECISION
    5000_PROCESS_AUTH --> 7100_SEND_RESPONSE
    5000_PROCESS_AUTH --> 8000_WRITE_AUTH_TO_DB
    5100_READ_XREF_RECORD --> 9500_LOG_ERROR
    5100_READ_XREF_RECORD -.->|reads| WS_CCXREF_FILE__ext
    5200_READ_ACCT_RECORD --> 9500_LOG_ERROR
    5200_READ_ACCT_RECORD -.->|reads| WS_ACCTFILENAME__ext
    5300_READ_CUST_RECORD --> 9500_LOG_ERROR
    5300_READ_CUST_RECORD -.->|reads| WS_CUSTFILENAME__ext
    5500_READ_AUTH_SUMMRY --> 9500_LOG_ERROR
    7100_SEND_RESPONSE --> 9500_LOG_ERROR
    7100_SEND_RESPONSE -.->|calls| MQPUT1__ext
    8000_WRITE_AUTH_TO_DB --> 8400_UPDATE_SUMMARY
    8000_WRITE_AUTH_TO_DB --> 8500_INSERT_AUTH
    8400_UPDATE_SUMMARY --> 9500_LOG_ERROR
    8500_INSERT_AUTH --> 9500_LOG_ERROR
    9000_TERMINATE --> 9100_CLOSE_REQUEST_QUEUE
    9100_CLOSE_REQUEST_QUEUE --> 9500_LOG_ERROR
    9100_CLOSE_REQUEST_QUEUE -.->|calls| MQCLOSE__ext
    9500_LOG_ERROR --> 9990_END_ROUTINE
    9990_END_ROUTINE --> 9000_TERMINATE
    MAIN_PARA --> 1000_INITIALIZE
    MAIN_PARA --> 2000_MAIN_PROCESS
    MAIN_PARA --> 9000_TERMINATE
```

## Open Questions

- ? What is the structure of the MQ messages?
  - Context: The program reads from and writes to MQ queues, but the exact message format is not defined in the provided code snippet.
- ? What is the purpose of 5600-READ-PROFILE-DATA?
  - Context: The paragraph contains only a CONTINUE statement and does not perform any meaningful operation.

## Sequence Diagram

```mermaid
sequenceDiagram
    MAIN-PARA->>1000-INITIALIZE: performs
    1000-INITIALIZE-->>MAIN-PARA: WS-REQUEST-QNAME, WS-TRIGGER-DATA, WS-WAIT-INTERVAL
    MAIN-PARA->>2000-MAIN-PROCESS: WS-REQSTS-PROCESS-LIMIT, WS-MSG-PROCESSED
    2000-MAIN-PROCESS-->>MAIN-PARA: WS-MSG-PROCESSED
    MAIN-PARA->>9000-TERMINATE: performs
    1000-INITIALIZE->>1100-OPEN-REQUEST-QUEUE: WS-REQUEST-QNAME, WS-OPTIONS
    1100-OPEN-REQUEST-QUEUE-->>1000-INITIALIZE: W01-HOBJ-REQUEST, WS-COMPCODE, WS-REASON
    1000-INITIALIZE->>3100-READ-REQUEST-MQ: WS-WAIT-INTERVAL, W01-HCONN-REQUEST, W01-HOBJ-REQUEST
    3100-READ-REQUEST-MQ-->>1000-INITIALIZE: WS-SAVE-CORRELID, WS-REPLY-QNAME, WS-COMPCODE, ...
    2000-MAIN-PROCESS->>2100-EXTRACT-REQUEST-MSG: W01-GET-BUFFER, W01-DATALEN
    2100-EXTRACT-REQUEST-MSG-->>2000-MAIN-PROCESS: PA-RQ-AUTH-DATE, PA-RQ-AUTH-TIME, PA-RQ-CARD-NUM, ...
    2000-MAIN-PROCESS->>5000-PROCESS-AUTH: performs
    5000-PROCESS-AUTH-->>2000-MAIN-PROCESS: APPROVE-AUTH, CARD-FOUND-XREF, FOUND-ACCT-IN-MSTR
    2000-MAIN-PROCESS->>3100-READ-REQUEST-MQ: WS-WAIT-INTERVAL, MQMI-NONE, MQCI-NONE, ...
    3100-READ-REQUEST-MQ-->>2000-MAIN-PROCESS: MQGMO-OPTIONS, MQMD-MSGID, MQMD-CORRELID, ...
    5000-PROCESS-AUTH->>1200-SCHEDULE-PSB: PSB-NAME
    1200-SCHEDULE-PSB-->>5000-PROCESS-AUTH: IMS-RETURN-CODE, IMS-PSB-SCHD
    5000-PROCESS-AUTH->>5100-READ-XREF-RECORD: PA-RQ-CARD-NUM, WS-CCXREF-FILE, WS-RESP-CD, ...
    5100-READ-XREF-RECORD-->>5000-PROCESS-AUTH: XREF-CARD-NUM, CARD-FOUND-XREF, CARD-NFOUND-XREF, ...
    5000-PROCESS-AUTH->>5200-READ-ACCT-RECORD: XREF-ACCT-ID, WS-ACCTFILENAME, WS-CARD-RID-ACCT-ID-X, ...
    5200-READ-ACCT-RECORD-->>5000-PROCESS-AUTH: WS-CARD-RID-ACCT-ID, FOUND-ACCT-IN-MSTR, NFOUND-ACCT-IN-MSTR, ...
    5000-PROCESS-AUTH->>5300-READ-CUST-RECORD: XREF-CUST-ID, WS-CUSTFILENAME, WS-CARD-RID-CUST-ID-X, ...
    5300-READ-CUST-RECORD-->>5000-PROCESS-AUTH: WS-CARD-RID-CUST-ID, FOUND-CUST-IN-MSTR, NFOUND-CUST-IN-MSTR, ...
    5000-PROCESS-AUTH->>5500-READ-AUTH-SUMMRY: XREF-ACCT-ID, PA-ACCT-ID
    5500-READ-AUTH-SUMMRY-->>5000-PROCESS-AUTH: FOUND-PAUT-SMRY-SEG, NFOUND-PAUT-SMRY-SEG, IMS-RETURN-CODE, ...
    5000-PROCESS-AUTH->>5600-READ-PROFILE-DATA: performs
    5000-PROCESS-AUTH->>6000-MAKE-DECISION: FOUND-PAUT-SMRY-SEG, PA-CREDIT-LIMIT, PA-CREDIT-BALANCE, ...
    6000-MAKE-DECISION-->>5000-PROCESS-AUTH: DECLINE-AUTH, APPROVE-AUTH, AUTH-RESP-DECLINED, ...
    5000-PROCESS-AUTH->>7100-SEND-RESPONSE: MQOT-Q, WS-REPLY-QNAME, MQMT-REPLY, ...
    7100-SEND-RESPONSE-->>5000-PROCESS-AUTH: WS-COMPCODE, WS-REASON, ERR-LOCATION, ...
    5000-PROCESS-AUTH->>8000-WRITE-AUTH-TO-DB: performs
    5100-READ-XREF-RECORD->>9500-LOG-ERROR: WS-CICS-TRANID, WS-PGM-AUTH, WS-CUR-DATE-X6, ...
    9500-LOG-ERROR-->>5100-READ-XREF-RECORD: WS-ABS-TIME
    5100-READ-XREF-RECORD->>9500-LOG-ERROR: WS-CICS-TRANID, WS-PGM-AUTH, WS-CUR-DATE-X6, ...
    9500-LOG-ERROR-->>5100-READ-XREF-RECORD: WS-ABS-TIME
    5100-READ-XREF-RECORD->>WS-CCXREF-FILE: performs
    5200-READ-ACCT-RECORD->>9500-LOG-ERROR: WS-CICS-TRANID, WS-PGM-AUTH, WS-CUR-DATE-X6, ...
    9500-LOG-ERROR-->>5200-READ-ACCT-RECORD: WS-ABS-TIME
    5200-READ-ACCT-RECORD->>9500-LOG-ERROR: WS-CICS-TRANID, WS-PGM-AUTH, WS-CUR-DATE-X6, ...
    9500-LOG-ERROR-->>5200-READ-ACCT-RECORD: WS-ABS-TIME
    5200-READ-ACCT-RECORD->>WS-ACCTFILENAME: performs
    5300-READ-CUST-RECORD->>9500-LOG-ERROR: WS-CICS-TRANID, WS-PGM-AUTH, WS-ABS-TIME, ...
    5300-READ-CUST-RECORD->>9500-LOG-ERROR: WS-CICS-TRANID, WS-PGM-AUTH, WS-ABS-TIME, ...
    5300-READ-CUST-RECORD->>WS-CUSTFILENAME: performs
    5500-READ-AUTH-SUMMRY->>9500-LOG-ERROR: WS-CICS-TRANID, WS-PGM-AUTH, WS-ABS-TIME, ...
    7100-SEND-RESPONSE->>MQPUT1: performs
    7100-SEND-RESPONSE->>9500-LOG-ERROR: WS-CICS-TRANID, WS-PGM-AUTH, WS-ABS-TIME, ...
    8000-WRITE-AUTH-TO-DB->>8400-UPDATE-SUMMARY: NFOUND-PAUT-SMRY-SEG, XREF-ACCT-ID, XREF-CUST-ID, ...
    8400-UPDATE-SUMMARY-->>8000-WRITE-AUTH-TO-DB: PENDING-AUTH-SUMMARY, IMS-RETURN-CODE
    8000-WRITE-AUTH-TO-DB->>8500-INSERT-AUTH: WS-ABS-TIME, WS-CUR-DATE-X6, WS-CUR-TIME-X6, ...
    8500-INSERT-AUTH-->>8000-WRITE-AUTH-TO-DB: PA-AUTH-DATE-9C, PA-AUTH-TIME-9C, PA-AUTH-ORIG-DATE, ...
    COPAUA0C->>CMQODV: performs
    COPAUA0C->>CMQMDV: performs
    COPAUA0C->>CMQODV: performs
    COPAUA0C->>CMQMDV: performs
    COPAUA0C->>CMQV: performs
    COPAUA0C->>CMQTML: performs
    COPAUA0C->>CMQPMOV: performs
    COPAUA0C->>CMQGMOV: performs
    COPAUA0C->>CCPAURQY: performs
    COPAUA0C->>CCPAURLY: performs
    COPAUA0C->>CCPAUERY: performs
    COPAUA0C->>CIPAUSMY: performs
    COPAUA0C->>CIPAUDTY: performs
    COPAUA0C->>CVACT03Y: performs
    COPAUA0C->>CVACT01Y: performs
    COPAUA0C->>CVCUS01Y: performs
    1100-OPEN-REQUEST-QUEUE->>MQOPEN: performs
    1100-OPEN-REQUEST-QUEUE->>9500-LOG-ERROR: WS-CICS-TRANID, WS-PGM-AUTH, WS-CUR-DATE-X6, ...
    9500-LOG-ERROR-->>1100-OPEN-REQUEST-QUEUE: WS-ABS-TIME
    1200-SCHEDULE-PSB->>9500-LOG-ERROR: WS-CICS-TRANID, WS-PGM-AUTH, WS-CUR-DATE-X6, ...
    9500-LOG-ERROR-->>1200-SCHEDULE-PSB: WS-ABS-TIME
    3100-READ-REQUEST-MQ->>MQGET: performs
    3100-READ-REQUEST-MQ->>9500-LOG-ERROR: WS-CICS-TRANID, WS-PGM-AUTH, WS-CUR-DATE-X6, ...
    8400-UPDATE-SUMMARY->>9500-LOG-ERROR: IMS-RETURN-CODE, ERR-LOCATION, ERR-CRITICAL, ...
    9500-LOG-ERROR-->>8400-UPDATE-SUMMARY: ERR-APPLICATION, ERR-PROGRAM, ERR-DATE, ...
    8500-INSERT-AUTH->>9500-LOG-ERROR: ERR-LOCATION, ERR-CRITICAL, ERR-IMS, ...
    9500-LOG-ERROR-->>8500-INSERT-AUTH: ERR-APPLICATION, ERR-PROGRAM, ERR-DATE, ...
    9000-TERMINATE->>9100-CLOSE-REQUEST-QUEUE: WS-REQUEST-MQ-OPEN
    9100-CLOSE-REQUEST-QUEUE-->>9000-TERMINATE: WS-REQUEST-MQ-CLSE
    9100-CLOSE-REQUEST-QUEUE->>MQCLOSE: performs
    9100-CLOSE-REQUEST-QUEUE->>9500-LOG-ERROR: WS-COMPCODE, WS-REASON, ERR-LOCATION, ...
    9500-LOG-ERROR-->>9100-CLOSE-REQUEST-QUEUE: ERR-APPLICATION, ERR-PROGRAM, ERR-DATE, ...
    9500-LOG-ERROR->>9990-END-ROUTINE: ERR-CRITICAL
    9990-END-ROUTINE->>9000-TERMINATE: IMS-PSB-SCHD
```
