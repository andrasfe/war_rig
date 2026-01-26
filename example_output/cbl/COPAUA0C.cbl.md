# COPAUA0C

**File**: `cbl/COPAUA0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 14:25:21.899998

## Purpose

COPAUA0C is a CICS transaction program that processes card authorization requests received via triggered MQ messages. It reads authorization request messages from an MQ request queue in a loop (up to 500 messages), extracts transaction details such as card number, amount, and merchant info, performs validation against cross-reference, account, customer files, and IMS database segments, decides on approval or decline based on business rules like funds availability and card status, and writes the authorization response to an IMS database. After processing, it commits via SYNCPOINT and potentially sends replies via MQ reply queue.

**Business Context**: CardDemo application - Authorization Module for processing payment card transaction authorizations

## Inputs

| Name | Type | Description |
|------|------|-------------|
| WS-REQUEST-QNAME | IOType.OTHER | MQ request queue containing comma-delimited authorization request messages with fields like auth date/time, card number, transaction amount, merchant details |
| CCXREF | IOType.FILE_VSAM | Card cross-reference file used to map card number to customer ID and account ID |
| ACCTDAT | IOType.FILE_VSAM | Account master file (inferred from WS-ACCTFILENAME; read not in sample but cited in challenger as line 278) |
| CUSTDAT | IOType.FILE_VSAM | Customer master file (inferred from WS-CUSTFILENAME; read not in sample but cited in challenger as line 325) |
| PSBPAUTB PAUT-SMRY-SEG | IOType.IMS_SEGMENT | IMS database segment for authorization summary (flagged as FOUND-PAUT-SMRY-SEG) |
| CARDDAT | IOType.FILE_VSAM | Card master file (defined but no read operation visible in sampled code) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| WS-REPLY-QNAME | IOType.OTHER | MQ reply queue for authorization responses (inferred from WS-REPLY-QNAME and program function; put operation not sampled) |
| PAUT IMS DB | IOType.IMS_SEGMENT | IMS authorization record written with response details including auth codes, dates, amounts, and merchant info |

## Business Rules

- **BR001**: Decline authorization if insufficient funds
- **BR002**: Decline if card not active
- **BR003**: Decline if account closed
- **BR004**: Decline if card or merchant fraud flagged
- **BR005**: Process up to 500 requests per invocation

## Paragraphs/Procedures

### 1000-MAIN
This is the primary entry point paragraph that orchestrates the overall program flow for batch-like processing of MQ authorization requests under CICS. It begins by retrieving the trigger message from the CICS transient queue MQTM to obtain the request queue name and trigger data (original lines 233-240). It sets a wait interval and performs 1100-OPEN-REQUEST-QUEUE to open the MQ request queue for input-shared access. It then initiates the first read with 3100-READ-REQUEST-MQ and enters an implicit loop via later PERFORMs. Inputs consumed include the CICS EIB and MQ trigger message; no files opened here. Outputs include initialized queue handles and flags. Business logic involves setting up for looped message processing up to WS-REQSTS-PROCESS-LIMIT (500). No explicit error handling here beyond NOHANDLE on RETRIEVE. It calls 1100-OPEN-REQUEST-QUEUE for MQ open and 3100-READ-REQUEST-MQ to start the message loop. Control passes to 2000-LOOP-MAIN after first read.

### 1100-OPEN-REQUEST-QUEUE
This paragraph opens the MQ request queue for shared input to enable reading authorization messages. It consumes WS-REQUEST-QNAME from the trigger message and sets MQOD fields like OBJECTTYPE to MQOT-Q and OBJECTNAME. Computes options as MQOO-INPUT-SHARED for non-exclusive access suitable for triggered processing. Performs MQOPEN call (inferred from context and omitted lines). Inputs are WS-REQUEST-QNAME and MQ constants; outputs updated MQMD/MQOD structures and connection objects like W01-HCONN-REQUEST. No business decisions, but sets WS-REQUEST-MQ-FLG to 'O'. Error handling via MQ compcode/reason codes (WS-COMPCODE, WS-REASON) not shown in sample. Calls MQOPEN routine (unsampled). Essential for enabling subsequent MQGET in 3100.

### 2000-LOOP-MAIN
This paragraph implements the main processing loop for multiple authorization requests, extracting, processing, committing, and looping until limit or no more messages. It consumes the MQ buffer from prior GET, performs 2100-EXTRACT-REQUEST-MSG to parse into PA-RQ-* fields, then 5000-PROCESS-AUTH for validation and decision. Increments WS-MSG-PROCESSED counter and issues SYNCPOINT to commit changes. Inputs from prior MQ read and flags like WS-MSG-AVAILABLE-FLG; outputs updated counters and IMS writes. Business logic checks if WS-MSG-PROCESSED exceeds 500, setting WS-LOOP-END if true, else loops back to 3100-READ-REQUEST-MQ. Handles loop termination cleanly. Error handling via subordinate returns and IMS-PSB-NOT-SCHD reset. Calls 2100-EXTRACT-REQUEST-MSG, 5000-PROCESS-AUTH, and conditionally 3100-READ-REQUEST-MQ. Ensures bounded processing to avoid indefinite loops.

### 2100-EXTRACT-REQUEST-MSG
This paragraph parses the raw MQ message buffer into structured authorization request fields using UNSTRING. It consumes W01-GET-BUFFER(1:W01-DATALEN) delimited by commas, populating PA-RQ-AUTH-DATE, PA-RQ-CARD-NUM, PA-RQ-TRANSACTION-AMT-AN, merchant fields, etc. Then converts WS-TRANSACTION-AMT-AN to numeric PA-RQ-TRANSACTION-AMT via NUMVAL. Inputs are raw comma-delimited string from MQGET; outputs populated PA-RQ-* working fields for downstream use. No conditions or decisions, purely data transformation. No error handling shown (assumes well-formed messages). No calls to other paragraphs. Prepares data for 5000-PROCESS-AUTH validations.

### 5000-PROCESS-AUTH
This core processing paragraph handles individual authorization decisions by reading cross-ref, account, customer data, and IMS segments, applying rules, and preparing response. It consumes extracted PA-RQ-* fields, performs 5100-READ-XREF-RECORD to get cust/acct IDs if card found. Conditionally reads ACCT/CUST masters (unsampled lines 278,325) and IMS PAUT-SMRY-SEG, computes available amt WS-AVAILABLE-AMT vs transaction. Sets flags like AUTH-RESP-APPROVED/DECLINED, decline reasons (insufficient, not active, etc.). If approved or certain conditions, performs 8000-WRITE-AUTH-TO-DB. Inputs from PA-RQ-*, XREF, masters, IMS; outputs flags WS-DECLINE-FLG, response fields PA-RL-*, IMS write. Business logic: fraud checks, balance validation, status checks leading to approve/decline. Error handling via file/IMS status flags like CARD-FOUND-XREF. Calls 5100-READ-XREF-RECORD and conditionally 8000-WRITE-AUTH-TO-DB. Determines final auth outcome.

### 5100-READ-XREF-RECORD
This paragraph reads the card cross-reference VSAM record using CICS READ DATASET with RIDFLD. Consumes PA-RQ-CARD-NUM moved to XREF-CARD-NUM. Performs READ into CARD-XREF-RECORD, captures RESP/RESP2. Evaluates WS-RESP-CD: NORMAL sets CARD-FOUND-XREF, extracts cust/acct IDs; NOTFND sets CARD-NFOUND-XREF and NFOUND-ACCT-IN-MSTR. Inputs card num from request; outputs XREF record fields or not-found flags. Business logic distinguishes found vs notfound for subsequent acct/cust reads. Error handling via DFHRESP evaluation for NORMAL/NOTFND. No calls. Enables mapping card to acct/cust.

## Open Questions

- ? Are there CICS READ operations for ACCTDAT (line 35), CUSTDAT (line 36), CARDDAT (line 37), CARDAIX (line 38)?
  - Context: No read statements visible in sampled code; challenger notes at lines 278 (ACCT), 325 (CUST), but samples omit those sections
- ? Details of 3100-READ-REQUEST-MQ, 8000-WRITE-AUTH-TO-DB, MQPUT to reply queue?
  - Context: Performs cited but code omitted in samples
- ? IMS GU/GETV calls for PAUT-SMRY-SEG and other reads/writes?
  - Context: PSB and flags present (lines 99-136), but no EXEC DL/I shown in sample
- ? Copybook definitions for CARD-XREF-RECORD, PA-RQ-*, PA-* fields?
  - Context: Fields used extensively but no COPY statements in sample
