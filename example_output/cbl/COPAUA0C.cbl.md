# COPAUA0C

**File**: `cbl/COPAUA0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-27 02:40:21.081116

## Purpose

COPAUA0C is a CICS COBOL program in the CardDemo application that performs card authorization decisions. It processes incoming authorization requests received via IBM MQ, validates against VSAM files (ACCTDAT, CUSTDAT, CARDDAT, CCXREF) and IMS database segments (PAUT PCB), and sends responses via MQ reply queue. The program applies business rules for approval/decline based on account status, funds availability, fraud flags, and other conditions.

**Business Context**: CardDemo Authorization Module for processing payment card authorization requests

## Inputs

| Name | Type | Description |
|------|------|-------------|
| WS-REQUEST-QNAME | IOType.OTHER | MQ queue for incoming pending authorization requests, parsed into PENDING-AUTH-REQUEST structure |
| PAUT-PCB | IOType.IMS_SEGMENT | IMS database PCB for reading pending authorization summary (root) and details (child) segments |
| ACCTDAT | IOType.FILE_VSAM | Account master file for account status and balance validation |
| CUSTDAT | IOType.FILE_VSAM | Customer master file for customer details validation |
| CARDDAT | IOType.FILE_VSAM | Card master file for card details and status |
| CCXREF | IOType.FILE_VSAM | Card cross-reference file linking card to customer/account |
| DFHCOMMAREA | IOType.CICS_COMMAREA | CICS linkage commarea for program communication |
| REQUEST-MQ-QUEUE | IOType.OTHER | IBM MQ queue containing comma-delimited authorization request messages with fields like date, time, card number, amount, merchant details |
| WS-CCXREF-FILE | IOType.FILE_VSAM | Card cross-reference VSAM file keyed by card number to get account and customer IDs |
| WS-ACCTFILENAME | IOType.FILE_VSAM | Account master VSAM file keyed by account ID for credit limits and balances |
| WS-CUSTFILENAME | IOType.FILE_VSAM | Customer master VSAM file keyed by customer ID |
| PAUT-PCB-NUM | IOType.IMS_SEGMENT | IMS PCB for PAUT database, segments PAUTSUM0 (pending auth summary), PAUTDTL1 (pending auth details) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| WS-REPLY-QNAME | IOType.OTHER | MQ queue for sending authorization responses populated from PENDING-AUTH-RESPONSE |
| PAUT-PCB | IOType.IMS_SEGMENT | IMS database for writing updated pending authorization summary and details segments |
| CCPAUERY | IOType.OTHER | Application error log structure for logging errors |
| REPLY-MQ-QUEUE | IOType.OTHER | IBM MQ reply queue receiving comma-delimited response with card num, transaction ID, auth code, response code, reason, approved amount |
| CSSL | IOType.CICS_QUEUE | CICS transient data queue for error logging with details like location, codes, message, event key |

## Business Rules

- **BR001**: Decline authorization if insufficient funds
- **BR002**: Decline authorization if card not active
- **BR003**: Decline authorization if account closed
- **BR004**: Decline authorization if card fraud flagged
- **BR005**: Decline authorization if merchant fraud
- **BR006**: Decline authorization if transaction amount exceeds available credit (credit limit minus balance)
- **BR007**: Set authorization response code and reason based on decline conditions like not found records, insufficient funds, fraud, etc.

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point paragraph that orchestrates the entire program flow for processing authorization requests. It consumes no direct inputs but relies on CICS environment including TS queue for trigger data. It sequentially performs initialization to open MQ queue and read first message, then main processing loop, and finally termination to close resources before CICS RETURN. No business decisions are made here; it delegates to subordinate paragraphs. Error handling is implicit via performed paragraphs. It calls 1000-INITIALIZE for setup including MQ open and first read, 2000-MAIN-PROCESS for loop of message processing, and 9000-TERMINATE for cleanup.

### 1000-INITIALIZE
This paragraph handles program initialization by retrieving trigger message from CICS TS queue into MQTM, extracting queue name and trigger data. It sets wait interval and performs opening the request MQ queue and reading the first request message. Inputs are CICS TS queue via RETRIEVE and MQ queue via MQGET in sub-paragraphs. Outputs are working storage flags like WS-REQUEST-MQ-OPEN and initial message buffer. No business logic or decisions; purely setup. Errors from sub-calls propagate via error flags. Calls 1100-OPEN-REQUEST-QUEUE to open MQ and 3100-READ-REQUEST-MQ for first message.

### 1100-OPEN-REQUEST-QUEUE
Prepares and calls MQOPEN to open the request queue in input-shared mode using queue name from trigger message. Inputs: WS-REQUEST-QNAME from MQTM-QNAME. Outputs: handle in W01-HOBJ-REQUEST if successful, sets WS-REQUEST-MQ-OPEN flag. On failure (non-MQCC-OK), sets error flags, populates error record with codes and 'REQ MQ OPEN ERROR', performs 9500-LOG-ERROR. No business logic. Called during init.

### 1200-SCHEDULE-PSB
Schedules the IMS PSB named in PSB-NAME using DLI SCHD, handling if already scheduled by TERM then reschedule. Inputs: PSB-NAME working storage. Outputs: sets IMS-PSB-SCHD flag if status OK. On failure, sets critical IMS error, logs via 9500-LOG-ERROR. Ensures PSB available for later DL/I calls. Called in processing.

### 2000-MAIN-PROCESS
Main loop performs until no more messages or loop end: extracts message, processes auth, increments count, syncpoint, unschedule PSB, checks process limit then reads next or ends. Inputs: messages from prior 3100 read. Outputs: processed count WS-MSG-PROCESSED, database updates via process, MQ responses. Business logic: limits processing to WS-REQSTS-PROCESS-LIMIT. Errors handled in subs. Calls extract, process auth, read next.

### 2100-EXTRACT-REQUEST-MSG
Parses comma-delimited buffer from MQGET into PA-RQ-* fields using UNSTRING, converts transaction amount AN to numeric. Inputs: W01-GET-BUFFER from MQ read. Outputs: populated PA-RQ- fields like CARD-NUM, AMT, MERCHANT-ID for processing, WS-TRANSACTION-AMT. No decisions or errors here. Prepares data for auth logic.

### 3100-READ-REQUEST-MQ
Calls MQGET with wait, no-syncpoint, convert options to read next message into buffer. Inputs: open queue handles. Outputs: message data, correlid, replyqname saved; sets NO-MORE-MSG-AVAILABLE on MQRC-NO-MSG. On other errors, critical CICS error log with 'FAILED TO READ REQUEST MQ'. Loops until no msg or limit.

### 5000-PROCESS-AUTH
Core processing: assumes approve, schedules PSB, assumes found records, reads xref/acct/cust/authsum/profile (profile empty), makes decision, sends response, conditionally writes to DB. Inputs: parsed PA-RQ- data. Outputs: response MQ, IMS updates, flags like APPROVE-AUTH/DECLINE-AUTH. Business logic delegated to 6000. Errors via reads. Calls multiple read/update paragraphs.

### 5100-READ-XREF-RECORD
CICS READ VSAM CCXREF by card num into CARD-XREF-RECORD. Sets CARD-FOUND-XREF or NFOUND, propagates NFOUND-ACCT-IN-MSTR. Logs warning A001 if notfnd, critical C001 other resp. Inputs: PA-RQ-CARD-NUM. Outputs: XREF-ACCT-ID etc if found.

### 5200-READ-ACCT-RECORD
CICS READ VSAM account file by acct ID from xref. Sets FOUND-ACCT-IN-MSTR or NFOUND. Logs warning A002 notfnd, critical C002 other.

### 5300-READ-CUST-RECORD
CICS READ VSAM customer file by cust ID from xref. Sets FOUND-CUST-IN-MSTR or NFOUND. Logs warning A003 notfnd, critical C003 other.

### 5500-READ-AUTH-SUMMRY
DL/I GU PAUTSUM0 by acct ID into PENDING-AUTH-SUMMARY. Sets FOUND/NFOUND-PAUT-SMRY-SEG. Critical I002 other status.

### 5600-READ-PROFILE-DATA
Empty implementation, just CONTINUE. Intended for profile data read but not implemented in this chunk.

### 6000-MAKE-DECISION
Populates response fields PA-RL-*, computes available amt from summary or acct, declines if amt exceeded or not found, sets resp code '00'/'05', reason based on flags (3100 notfnd, 4100 insuff, etc.), strings response buffer. Inputs: all read data/flags. Outputs: PA-RL- fields, W02-PUT-BUFFER. Key business logic for auth decision.

### 7100-SEND-RESPONSE
Prepares MQPUT1 to reply queue using saved correlid, strings buffer. On fail, critical M004 MQ error log.

### 8000-WRITE-AUTH-TO-DB
Updates/inserts IMS pending auth summary and inserts details if card found in xref.

### 8400-UPDATE-SUMMARY
Initializes summary if not found, sets limits from acct, increments approved/declined cnts/amts, REPL if found else ISRT PAUTSUM0. Critical I003 fail.

### 8500-INSERT-AUTH
Gets current time/date, computes inverted timestamps, populates PENDING-AUTH-DETAILS from request/response, sets match flags, ISRT PAUTDTL1 under PAUTSUM0. Critical I004 fail.

### 9000-TERMINATE
Terms IMS PSB if scheduled, closes request queue.

### 9100-CLOSE-REQUEST-QUEUE
Calls MQCLOSE if open, warning M005 if fail.

### 9500-LOG-ERROR
Formats time/date, populates ERROR-LOG-RECORD, WRITEQ to CSSL TD queue. If critical, performs 9990-END-ROUTINE (not shown).

### 9990-END-ROUTINE
The 9990-END-ROUTINE paragraph serves as the primary termination handler in the COPAUA0C CICS program, controlling the final steps before exiting back to CICS. Its role in the overall program flow is to orchestrate end-of-task cleanup and return control to the transaction manager. It consumes no explicit inputs or data, relying instead on the global state of working storage and files established by prior paragraphs (not shown in this chunk). It produces no direct data outputs but triggers actions in the called 9000-TERMINATE paragraph, which is expected to perform tasks such as closing files, resetting flags, or logging final status. There is no business logic or conditional decision-making within this paragraph; execution is strictly sequential without IF statements or validations. No error handling is explicitly implemented here, though any issues from 9000-TERMINATE would propagate implicitly. This paragraph calls the 9000-TERMINATE paragraph solely for standardized termination procedures. After the PERFORM, it executes an unconditional EXEC CICS RETURN to end the task. The paragraph concludes with a scope terminator period, ensuring proper COBOL syntax.

### 9990-EXIT
The 9990-EXIT paragraph acts as a simple exit label and transfer point within the COPAUA0C program, likely targeted by GO TO statements from other paragraphs for branching to termination logic. Its primary role in program flow is to provide a clean, named endpoint that immediately transfers control via EXIT without further processing. It consumes no inputs, data, files, or variables. It produces no outputs or modifications to any data areas. There are no business rules, conditions, validations, or decisions implemented, as it contains only the EXIT statement. No error handling is present, as it performs no operations that could fail. It calls no other paragraphs or programs. The EXIT statement relinquishes control to the next executable statement in the PROCEDURE DIVISION or effectively ends the current flow if at the end. This minimal structure ensures reliable control flow redirection without side effects.

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

- ? No PROCEDURE DIVISION visible in chunk 1/3
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Exact file organizations (e.g., VSAM KSDS) and queue names values
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Specific field mappings in copybooks
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Data division and copybooks definitions
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? Details of 5600-READ-PROFILE-DATA and 9990-END-ROUTINE
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? Exact queue names, file names values
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? Calling transaction ID
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? What is the overall purpose and main processing logic of COPAUA0C?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
- ? What does the 9000-TERMINATE paragraph perform?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
- ? What are the data structures, files, copybooks, and variables used?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
- ? What are the inputs, outputs, entry points, and calling context?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
