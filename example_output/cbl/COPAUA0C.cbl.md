# COPAUA0C

**File**: `cbl/COPAUA0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-27 23:05:46.053388

## Purpose

This COBOL program, COPAUA0C, is a Card Authorization Decision Program designed for a CICS COBOL IMS MQ environment. It handles card authorization requests, interacts with IMS databases, and uses MQ for messaging. The program extracts request messages, processes authorization logic, and sends responses via MQ.

**Business Context**: Card authorization processing within a financial transaction system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| W01-GET-BUFFER | IOType.CICS_QUEUE | Input message from MQ containing card authorization request details. |
| IMS Database | IOType.IMS_SEGMENT | IMS database segments containing cardholder account and profile data. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| W02-PUT-BUFFER | IOType.CICS_QUEUE | Output message to MQ containing the card authorization response. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| MQOPEN | CallType.STATIC_CALL | Opens the request queue for reading messages. |
| MQGET | CallType.STATIC_CALL | Retrieves a message from the request queue. |
| MQPUT1 | CallType.STATIC_CALL | Sends a reply message to the reply queue. |

## Business Rules

- **BR001**: If the MQOPEN call fails, log an error.
- **BR002**: If the MQGET call fails due to no message available, set the NO-MORE-MSG-AVAILABLE flag.
- **BR003**: If the MQPUT1 call fails, log an error.

## Paragraphs/Procedures

### 1100-OPEN-REQUEST-QUEUE
This paragraph opens the MQ request queue, enabling the program to receive authorization requests. It moves the queue name (WS-REQUEST-QNAME) and object type (MQOT-Q) into the MQ object descriptor (MQM-OD-REQUEST). It then computes the options for opening the queue, setting it to input-shared mode. The MQOPEN call is then made, using the connection handle, object descriptor, options, object handle, completion code, and reason code. If the MQOPEN call is successful (WS-COMPCODE = MQCC-OK), the WS-REQUEST-MQ-OPEN flag is set to TRUE. If the call fails, an error is logged using paragraph 9500-LOG-ERROR, including the completion code and reason code. The paragraph consumes WS-REQUEST-QNAME and MQOT-Q as inputs, and produces either a successful queue opening or an error log entry. It calls the MQOPEN program and potentially the 9500-LOG-ERROR paragraph.

### 1200-SCHEDULE-PSB
This paragraph schedules a PSB (Program Specification Block) for IMS database interaction. It uses the EXEC DLI SCHD command with the PSB-NAME. If the PSB has been scheduled more than once (PSB-SCHEDULED-MORE-THAN-ONCE), it terminates the PSB using EXEC DLI TERM and then reschedules it. The IMS-RETURN-CODE is updated with the DIBSTAT value after each scheduling attempt. This paragraph ensures the program can access the necessary IMS database resources. The paragraph consumes PSB-NAME as input and updates IMS-RETURN-CODE. It calls EXEC DLI SCHD and EXEC DLI TERM. The purpose of scheduling the PSB is to prepare the IMS environment for subsequent database operations related to card authorization processing. Error handling is implicit in the IMS environment, with DIBSTAT providing status information.

### 2100-EXTRACT-REQUEST-MSG
This paragraph extracts the card authorization request message from the W01-GET-BUFFER. It uses the UNSTRING statement to parse the message, which is delimited by commas. The extracted data elements are moved into corresponding fields within the PA-RQ-* data structure, such as PA-RQ-AUTH-DATE, PA-RQ-CARD-NUM, and PA-RQ-TRANSACTION-AMT. The alphanumeric transaction amount (WS-TRANSACTION-AMT-AN) is converted to a numeric value (PA-RQ-TRANSACTION-AMT) using the FUNCTION NUMVAL. This paragraph consumes W01-GET-BUFFER and produces the parsed data elements in the PA-RQ-* fields. The purpose is to prepare the request data for subsequent authorization processing. No explicit error handling is shown in the provided code snippet for the UNSTRING operation, but it is assumed that the message format is validated elsewhere.

### 3100-READ-REQUEST-MQ
This paragraph reads a message from the MQ request queue. It sets the MQGMO-OPTIONS for the MQGET call, including MQGMO-NO-SYNCPOINT, MQGMO-WAIT, MQGMO-CONVERT, and MQGMO-FAIL-IF-QUIESCING. It also sets the MQMD fields, such as MSGID and CORRELID. The MQGET call is then made, using the connection handle, object handle, message descriptor, get message options, buffer length, buffer, data length, completion code, and reason code. If the MQGET call is successful (WS-COMPCODE = MQCC-OK), the correlation ID and reply queue name are saved. If the call fails and the reason code is MQRC-NO-MSG-AVAILABLE, the NO-MORE-MSG-AVAILABLE flag is set to TRUE. Otherwise, an error is logged using paragraph 9500-LOG-ERROR. This paragraph consumes the MQ request queue and produces a message in W01-GET-BUFFER. It calls the MQGET program and potentially the 9500-LOG-ERROR paragraph.

### 5000-PROCESS-AUTH
This paragraph orchestrates the authorization processing. It first sets the APPROVE-AUTH flag to TRUE. It then schedules the PSB using paragraph 1200-SCHEDULE-PSB. After scheduling the PSB, it sets CARD-FOUND-XREF and FOUND-ACCT-IN-MSTR to TRUE. It then calls several paragraphs to read data: 5100-READ-XREF-RECORD, 5200-READ-ACCT-RECORD, 5300-READ-CUST-RECORD, 5500-READ-AUTH-SUMMRY, and 5600-READ-PROFILE-DATA. These read operations are performed only if CARD-FOUND-XREF is TRUE. This paragraph consumes the request data and produces the authorization decision. It calls 1200-SCHEDULE-PSB and several read paragraphs. The purpose is to gather all necessary data for making an authorization decision based on card and account information. The paragraph assumes that the called paragraphs will handle their own error conditions.

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

- ? What are the exact structures of the MQ message descriptors and options copybooks?
  - Context: The copybook names are not provided, making it impossible to determine the precise layout of the MQ structures.
- ? What is the purpose and implementation of the 9500-LOG-ERROR paragraph?
  - Context: This paragraph is called in several error handling routines, but its code is not included in the sampled source.
- ? What are the specific IMS database segments accessed by the read paragraphs (5100-5600)?
  - Context: The code only shows the paragraph calls, not the actual IMS database operations.
