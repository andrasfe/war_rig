# COPAUA0C

**File**: `cbl/COPAUA0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 17:37:23.676480

## Purpose

COPAUA0C is a CICS COBOL program in the CardDemo application serving as the Authorization Module for card authorization decisions. It integrates IMS database access via PSBPAUTB, VSAM files for account/customer/card data, and IBM MQ for request/reply queuing between pending authorization requests and responses. The program is invoked via CICS transaction ID CP00.

**Business Context**: Card payment authorization processing in a demonstration banking/payment system, handling validation against master records and IMS segments for approve/decline decisions.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| WS-REQUEST-QNAME | IOType.OTHER | MQ queue containing pending authorization requests (PENDING-AUTH-REQUEST layout) |
| WS-CCXREF-FILE (CCXREF) | IOType.FILE_VSAM | Card cross-reference file for mapping card to customer/account IDs |
| WS-ACCTFILENAME (ACCTDAT) | IOType.FILE_VSAM | Account master file records |
| WS-CUSTFILENAME (CUSTDAT) | IOType.FILE_VSAM | Customer master file records |
| WS-CARDFILENAME (CARDDAT) | IOType.FILE_VSAM | Card data file |
| PAUT-PCB (PSBPAUTB) | IOType.IMS_SEGMENT | IMS database segments for pending authorization summary and details |
| DFHCOMMAREA | IOType.CICS_COMMAREA | CICS linkage area for program communication (4096 bytes) |
| REQUEST-MQ-QUEUE | IOType.OTHER | IBM MQ queue containing comma-delimited authorization request messages with fields like date, time, card num, amount, merchant details |
| MQTM | IOType.CICS_COMMAREA | Trigger message retrieved from CICS to get request queue name and trigger data |
| WS-CCXREF-FILE | IOType.FILE_VSAM | Card cross-reference VSAM file, read by card number to get account and customer IDs |
| WS-ACCTFILENAME | IOType.FILE_VSAM | Account master VSAM file, read by account ID from XREF |
| WS-CUSTFILENAME | IOType.FILE_VSAM | Customer master VSAM file, read by customer ID from XREF |
| PAUT-PCB-NUM | IOType.IMS_SEGMENT | IMS PAUT database PCB for GU/ISRT/REPL on PAUTSUM0 (pending auth summary) and PAUTDTL1 (details) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| WS-REPLY-QNAME | IOType.OTHER | MQ reply queue for authorization responses (approved/declined with reasons) |
| Unnamed error log | IOType.OTHER | Application error log layout |
| REPLY-MQ-QUEUE | IOType.OTHER | IBM MQ reply queue, receives comma-delimited response with card num, trans ID, resp code, reason, approved amt |
| CSSL | IOType.CICS_QUEUE | CICS transient data queue for error logging, writes ERROR-LOG-RECORD on errors |
| PAUT-PCB-NUM | IOType.IMS_SEGMENT | IMS updates/inserts to PAUTSUM0 summary segment and PAUTDTL1 detail segment |

## Business Rules

- **BR001**: Authorization decline reasons defined: insufficient funds, card not active, account closed, card fraud, merchant fraud
- **BR002**: Process limit of 500 requests
- **BR003**: Decline authorization if transaction amount exceeds available credit: for existing pending auth summary, avail = credit limit - credit balance; else use account master credit limit - curr bal
- **BR004**: Set auth response code '00' approved or '05' declined, with approved amt = trans amt or 0; reason code based on decline cause like 3100 not found, 4100 insuff funds, etc.
- **BR005**: Process only up to WS-REQSTS-PROCESS-LIMIT messages per invocation

## Paragraphs/Procedures

### MAIN-PARA
MAIN-PARA serves as the primary entry point and orchestration paragraph controlling the entire program flow in this CICS task. It consumes no direct inputs but relies on EIB and CICS environment for trigger data. It sequentially performs initialization (open MQ, read first msg), main processing loop (extract, process auth, send response, commit), and termination (close IMS PSB and MQ). No business decisions are made here; it delegates to subordinates. Errors are handled implicitly via flags set in callees leading to log and potential abend. It calls 1000-INITIALIZE for setup including queue open and first read, 2000-MAIN-PROCESS for looped message handling with SYNCPOINT per msg, and 9000-TERMINATE for cleanup before CICS RETURN. After all performs, it executes CICS RETURN to end the task.

### 1000-INITIALIZE
1000-INITIALIZE handles program startup by retrieving MQ trigger message from CICS, setting queue name and wait interval, opening the request MQ queue, and reading the first request message. It consumes CICS EIBRESP and MQTM trigger data via RETRIEVE NOHANDLE. It produces WS-REQUEST-QNAME, WS-TRIGGER-DATA, WS-REQUEST-MQ-OPEN flag, and first W01-GET-BUFFER message. No business logic decisions; focuses on setup. Errors from MQOPEN set critical error flags and log via 9500-LOG-ERROR, but continues. Calls 1100-OPEN-REQUEST-QUEUE to perform the MQOPEN call and set flags, and 3100-READ-REQUEST-MQ for initial message get.

### 1100-OPEN-REQUEST-QUEUE
1100-OPEN-REQUEST-QUEUE opens the IBM MQ request queue using MQOPEN with input-shared options. It consumes WS-REQUEST-QNAME from trigger data to populate MQOD. It produces W01-HOBJ-REQUEST handle and sets WS-REQUEST-MQ-OPEN flag if successful. No conditions or business logic beyond success check. On MQCC-OK, sets open flag; else sets critical ERR-MQ flags, populates error codes/messages, and performs 9500-LOG-ERROR. No subordinate calls.

### 1200-SCHEDULE-PSB
1200-SCHEDULE-PSB schedules the IMS PSB for database access using DLI SCHD, handling if already scheduled by TERM then reschedule. It consumes PSB-NAME and DIBSTAT. It produces IMS-PSB-SCHD flag and IMS-RETURN-CODE. Checks STATUS-OK to set flag; else critical ERR-IMS and log. Ensures single scheduling via PSB-SCHEDULED-MORE-THAN-ONCE check. No subordinate calls.

### 2000-MAIN-PROCESS
2000-MAIN-PROCESS implements the main loop to process multiple auth requests until no more messages or process limit reached. It consumes messages from prior 3100-READ, WS-REQSTS-PROCESS-LIMIT, WS-MSG-PROCESSED counter. It produces incremented counter, commits per message via SYNCPOINT, unschedules PSB, and sets loop end flag. Business logic: loop condition NO-MORE-MSG-AVAILABLE OR WS-LOOP-END, limit check after each process. Errors handled in subordinates leading to log/abend. Calls 2100-EXTRACT-REQUEST-MSG to parse buffer, 5000-PROCESS-AUTH for full auth handling including reads/decision/response/db write, and re-calls 3100-READ-REQUEST-MQ if under limit.

### 2100-EXTRACT-REQUEST-MSG
2100-EXTRACT-REQUEST-MSG parses the MQGET buffer into PA-RQ-* request fields using UNSTRING delimited by comma. It consumes W01-GET-BUFFER(1:W01-DATALEN) from MQGET. It produces populated PA-RQ-AUTH-DATE thru PA-RQ-TRANSACTION-ID fields and WS-TRANSACTION-AMT via NUMVAL on amount. No conditions or errors handled directly. No business logic beyond parsing. No calls.

### 3100-READ-REQUEST-MQ
3100-READ-REQUEST-MQ performs non-destructive MQGET with wait interval, convert, no-syncpoint options to fetch next request message. It consumes W01-HCONN-REQUEST/HOBJ, WS-WAIT-INTERVAL. It produces W01-GET-BUFFER/DATALEN, WS-SAVE-CORRELID, WS-REPLY-QNAME if OK; sets NO-MORE-MSG-AVAILABLE on MQRC-NO-MSG-AVAILABLE. On other errors, critical ERR-CICS (misclassified?), log with card num if available. No business decisions. No calls.

### 5000-PROCESS-AUTH
5000-PROCESS-AUTH orchestrates single authorization request: assumes approve init, schedules IMS PSB, reads XREF/ACCT/CUST if found, reads IMS summary/profile (profile empty), makes decision, sends response, writes to DB if XREF found. Consumes parsed PA-RQ-*, VSAM/IMS records. Produces decision flags, response buffer, IMS updates. Business logic delegates decline conditions to reads/6000. Errors via subordinates. Calls 1200-SCHEDULE-PSB, 5100-READ-XREF-RECORD conditional chain, 5500/5600, 6000-MAKE-DECISION, 7100-SEND-RESPONSE, conditional 8000-WRITE-AUTH-TO-DB.

### 5100-READ-XREF-RECORD
5100-READ-XREF-RECORD reads VSAM CCXREF by card num, sets CARD-FOUND-XREF or NFOUND flags, propagates NFOUND-ACCT-IN-MSTR. Consumes PA-RQ-CARD-NUM. Produces CARD-XREF-RECORD, flags. EVALUATE RESP: NORMAL set found, NOTFND warning log 'A001' and set nfound, OTHER critical CICS log 'C001'. Calls 9500-LOG-ERROR on errors.

### 5200-READ-ACCT-RECORD
5200-READ-ACCT-RECORD conditionally called if XREF found, reads VSAM account by XREF-ACCT-ID, sets FOUND-ACCT-IN-MSTR or NFOUND. Consumes WS-CARD-RID-ACCT-ID from XREF. Produces ACCOUNT-RECORD, flags. EVALUATE RESP: NORMAL found, NOTFND warning 'A002', OTHER critical 'C002' log. Calls 9500-LOG-ERROR.

### 5300-READ-CUST-RECORD
5300-READ-CUST-RECORD conditionally called if XREF found, reads VSAM customer by XREF-CUST-ID, sets FOUND-CUST-IN-MSTR or NFOUND. Consumes WS-CARD-RID-CUST-ID from XREF. Produces CUSTOMER-RECORD, flags. EVALUATE RESP: NORMAL found, NOTFND warning 'A003', OTHER critical 'C003' log. Calls 9500-LOG-ERROR.

### 5500-READ-AUTH-SUMMRY
5500-READ-AUTH-SUMMRY performs IMS DLI GU on PAUTSUM0 segment by ACCT-ID for pending auth summary. Consumes PA-ACCT-ID from XREF. Produces PENDING-AUTH-SUMMARY, FOUND-PAUT-SMRY-SEG or NFOUND flags. EVALUATE DIBSTAT: OK found, SEG-NOT-FND nfound, OTHER critical IMS 'I002' log. Calls 9500-LOG-ERROR.

### 5600-READ-PROFILE-DATA
5600-READ-PROFILE-DATA is a placeholder paragraph that performs CONTINUE, implementing no logic or reads. Consumes nothing. Produces nothing. No conditions, errors, or calls. Likely stub for future profile reads.

### 6000-MAKE-DECISION
6000-MAKE-DECISION applies core auth rules: compute available credit from summary or acct, decline if trans amt exceeds, set resp code/reason based on flags, format response buffer via STRING. Consumes PA-RQ-*, summary/acct data, found flags. Produces PA-RL-* response fields, W02-PUT-BUFFER, WS-APPROVED-AMT. Business logic: nested IF for avail amt calc/decline, set '00'/'05' code, EVALUATE decline reasons for 3100/4100 etc. No errors direct. No calls.

### 7100-SEND-RESPONSE
7100-SEND-RESPONSE sends response to reply queue using MQPUT1 (implicit open), setting reply MD from saved correlid etc. Consumes WS-REPLY-QNAME, WS-SAVE-CORRELID, W02-PUT-BUFFER from decision. Produces message on queue if OK. Checks COMPCODE=OK else critical MQ 'M004' log. No business logic. No calls.

### 8000-WRITE-AUTH-TO-DB
8000-WRITE-AUTH-TO-DB conditionally called if XREF found, updates/inserts IMS summary and inserts auth detail. Consumes decision, acct/cust/summary data. Produces updated/inserted IMS segments. Delegates to 8400/8500. Errors via subordinates. Calls 8400-UPDATE-SUMMARY and 8500-INSERT-AUTH.

### 8400-UPDATE-SUMMARY
8400-UPDATE-SUMMARY prepares and REPL/ISRT PAUTSUM0 summary: init if nfound, set limits from acct, inc cnts/amts based on approve/decline, update bal if approved. Consumes found flags, acct data, approved amt. Produces updated PENDING-AUTH-SUMMARY. IF found REPL else ISRT, check STATUS-OK else critical IMS 'I003' log. Calls 9500-LOG-ERROR.

### 8500-INSERT-AUTH
8500-INSERT-AUTH computes inverted date/time keys via ASKTIME/FORMATTIME, populates PAUTH detail from request/response/XREF, sets match flag, ISRT PAUTDTL1 child under summary. Consumes PA-RQ-*, PA-RL-*, XREF-ACCT-ID. Produces PENDING-AUTH-DETAILS inserted. Check STATUS-OK else critical IMS 'I004' log. Calls 9500-LOG-ERROR.

### 9000-TERMINATE
9000-TERMINATE performs cleanup: unschedule IMS PSB if scheduled, close request MQ queue. Consumes IMS-PSB-SCHD flag, WS-REQUEST-MQ-OPEN. Sets WS-REQUEST-MQ-CLSE on close OK. Errors on close as warning MQ 'M005' log, no abend. Calls 9100-CLOSE-REQUEST-QUEUE.

### 9100-CLOSE-REQUEST-QUEUE
9100-CLOSE-REQUEST-QUEUE calls MQCLOSE on request queue handle if open. Consumes W01-HOBJ-REQUEST. Sets WS-REQUEST-MQ-CLSE if OK else warning ERR-MQ 'M005' log. Calls 9500-LOG-ERROR on error.

### 9500-LOG-ERROR
9500-LOG-ERROR formats and writes ERROR-LOG-RECORD to CICS TD queue 'CSSL' with timestamp, tranid, program, error details. Consumes populated ERR-* fields from callers, WS-CICS-TRANID etc. Produces log entry. If ERR-CRITICAL, performs 9990-END-ROUTINE (not in chunk). Uses ASKTIME/FORMATTIME NOHANDLE.

### 9990-END-ROUTINE
This paragraph serves as the primary end routine for terminating the COPAUA0C CICS program execution. Its role is to orchestrate the final cleanup and return control to CICS. It consumes no explicit inputs or data from files/variables visible in this chunk, relying instead on the program state established by prior paragraphs. It produces no direct outputs, but indirectly triggers any outputs from the called 9000-TERMINATE paragraph, such as potential file closes or log writes. The business logic is minimal, consisting solely of sequential execution without conditional decisions. No validation or error handling is performed within this paragraph itself. It calls the 9000-TERMINATE paragraph via PERFORM at line 10 to handle termination tasks like resource cleanup, which are not defined in this chunk. Following the PERFORM, it unconditionally executes the CICS RETURN at lines 12-13 to end the transaction and return to CICS. After the period at line 14, control naturally flows to the subsequent 9990-EXIT. This structure ensures a clean exit from the program.

### 9990-EXIT
This paragraph consists solely of an EXIT label and statement, serving as a standard exit point following the CICS RETURN in 9990-END-ROUTINE. Its primary role is to provide a conventional termination marker in the program flow, though execution would typically not reach here due to the prior CICS RETURN. It consumes no inputs or data from any sources. It produces no outputs or modifications to variables/files. No business logic, conditions, or decisions are implemented. No error handling or validation occurs. It makes no calls to other paragraphs or programs. The EXIT statement at line 16 simply transfers control back to the calling paragraph, if any, but in context, it acts as a fallback or structural endpoint.

## Open Questions

- ? Actual I/O operations (OPEN, READ, WRITE, GET, PUT for MQ/IMS/VSAM)
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Exact queue names populated in WS-REQUEST-QNAME and WS-REPLY-QNAME
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? File organization (confirmed VSAM/KSDS?) and access methods
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Program flow, paragraphs, and called programs
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Data division and working storage/copybook definitions
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? Details of 9990-END-ROUTINE and any other missing paragraphs
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? Reply MQ connection W02-HCONN-REPLY origin
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? WS-REQSTS-PROCESS-LIMIT value/source
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? Full list of decline flags like CARD-NOT-ACTIVE, ACCOUNT-CLOSED
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? What is the overall purpose and business function of the COPAUA0C program?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
- ? What does the 9000-TERMINATE paragraph do?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
- ? What inputs, outputs, copybooks, and data divisions are used?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
- ? What is the calling context, entry points, or linkage for this CICS program?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
