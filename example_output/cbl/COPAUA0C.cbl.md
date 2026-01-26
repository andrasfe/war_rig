# COPAUA0C

**File**: `cbl/COPAUA0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 15:11:36.825757

## Purpose

COPAUA0C is a CICS COBOL program in the CardDemo application that serves as the authorization decision engine for card transactions. It processes pending authorization requests received via IBM MQ, validates data against VSAM files (accounts, customers, cards, cross-references) and IMS database segments, applies approval/decline logic, updates IMS, and sends responses via MQ. The program operates as CICS transaction CP00.

**Business Context**: Card payment authorization processing in a demo banking/card application, handling transaction approvals/declines based on account status, balances, and fraud checks.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PENDING-AUTH-REQUEST | IOType.OTHER | Incoming MQ message containing pending authorization request data |
| ACCTDAT | IOType.FILE_VSAM | Account master records used for balance and status validation |
| CUSTDAT | IOType.FILE_VSAM | Customer master records for customer validation |
| CARDDAT | IOType.FILE_VSAM | Card data records, accessed via alternate index CARDAIX |
| CCXREF | IOType.FILE_VSAM | Card cross-reference file linking cardnum, cust-id, acct-id |
| PAUT | IOType.IMS_SEGMENT | IMS database segments for pending auth summary and details (PSB: PSBPAUTB, PCB: PAUT) |
| DFHCOMMAREA | IOType.CICS_COMMAREA | CICS linkage commarea for program communication |
| REQUEST-MQ | IOType.OTHER | IBM MQ queue containing comma-delimited authorization request messages with card num, amt, merchant data etc. |
| WS-CCXREF-FILE | IOType.FILE_VSAM | Card cross-reference VSAM file mapping card num to acct/cust IDs |
| WS-ACCTFILENAME | IOType.FILE_VSAM | Account master VSAM file containing credit limits and balances |
| WS-CUSTFILENAME | IOType.FILE_VSAM | Customer master VSAM file |
| PAUTSUM0 | IOType.IMS_SEGMENT | IMS pending authorization summary segment for account credit usage |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PENDING-AUTH-RESPONSE | IOType.OTHER | MQ response message with authorization decision (approve/decline) and reason |
| PENDING-AUTH-SUMMARY | IOType.IMS_SEGMENT | Root segment updated in IMS PAUT database |
| PENDING-AUTH-DETAILS | IOType.IMS_SEGMENT | Child segment inserted/updated in IMS PAUT database |
| CCPAUERY | IOType.OTHER | Application error log layout for logging errors |
| REPLY-MQ | IOType.OTHER | IBM MQ reply queue receiving comma-delimited auth response with resp code, reason, approved amt |
| CSSL | IOType.CICS_QUEUE | CICS transient data queue for error log records |
| PAUTSUM0 | IOType.IMS_SEGMENT | IMS pending auth summary segment updated/replaced with auth counts and balances |
| PAUTDTL1 | IOType.IMS_SEGMENT | IMS pending auth detail segment inserted with full auth transaction data |

## Business Rules

- **BR001**: Decline authorization request based on predefined reasons including insufficient funds, inactive card, closed account, card fraud, or merchant fraud
- **BR002**: Decline authorization if transaction amount exceeds available credit limit from auth summary or account master
- **BR003**: Set specific decline reason codes based on failure conditions like not found records or insufficient funds
- **BR004**: Approve auth sets resp code '00' and approved amt to trans amt; decline '05' with 0 amt

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point paragraph that orchestrates the entire program flow for the CICS task. It consumes no direct inputs but relies on CICS environment including EIB and trigger data. It sequentially performs initialization to setup MQ and read first message, then main process loop to handle multiple auth requests, followed by termination to cleanup resources, and finally issues CICS RETURN to end the task. No business decisions are made here; it delegates to subordinate paragraphs. Errors from subordinates are handled via flags and 9500-LOG-ERROR. It calls 1000-INITIALIZE for setup including MQ open and first read, 2000-MAIN-PROCESS for looping message processing, and 9000-TERMINATE for IMS TERM and MQ close.

### 1000-INITIALIZE
This paragraph handles program startup by retrieving CICS trigger message data to get queue names, setting wait interval, opening the request MQ queue, and performing initial read of first request message. It consumes CICS RETRIEVE into MQTM for queue name and trigger data (lines 20-23), and MQ connection handles from prior setup. It produces WS-REQUEST-QNAME, WS-TRIGGER-DATA, and populates first W01-GET-BUFFER via 3100 read. Business logic checks EIBRESP NORMAL to move queue data (24-27). Errors in MQ open trigger 9500-LOG-ERROR with critical flag (57-70). It calls 1100-OPEN-REQUEST-QUEUE to open MQ object, and 3100-READ-REQUEST-MQ for first message.

### 1100-OPEN-REQUEST-QUEUE
Prepares and calls MQOPEN to access the request queue for input-shared reading. Consumes WS-REQUEST-QNAME for object name (44-45), computes MQOO-INPUT-SHARED options (47). Produces W01-HOBJ-REQUEST handle if successful, sets WS-REQUEST-MQ-OPEN true (58). No business logic beyond success check. On MQCC failure, sets critical MQ error, populates ERR fields, performs 9500-LOG-ERROR (60-69). Calls no other paragraphs.

### 1200-SCHEDULE-PSB
Schedules the IMS PSB for database access using DLI SCHD, handling if already scheduled by TERM then reschedule. Consumes PSB-NAME. Produces IMS-PSB-SCHD flag if successful, sets IMS-RETURN-CODE from DIBSTAT (84,93). Logic checks PSB-SCHEDULED-MORE-THAN-ONCE to TERM and reschedule (85-92), then STATUS-OK check (95). Errors set critical IMS error and log (98-103). Called in 5000 for each auth to ensure PSB active.

### 2000-MAIN-PROCESS
Main loop processes multiple auth requests until no more messages or process limit reached. Consumes messages from prior 3100-READ-REQUEST-MQ into W01-GET-BUFFER. Extracts/parses data via 2100, processes auth via 5000 including reads/decisions/updates, increments count, SYNCPOINT commits, unschedules PSB, checks limit then re-reads or exits. Business logic: loop until NO-MORE-MSG-AVAILABLE or WS-LOOP-END or count > limit (113,126). Errors delegated to callees. Calls 2100-EXTRACT, 5000-PROCESS-AUTH, 3100-READ conditionally.

### 2100-EXTRACT-REQUEST-MSG
Parses the MQGET buffer into PA-RQ-* request fields using UNSTRING delimited by comma. Consumes W01-GET-BUFFER(1:W01-DATALEN) from 3100 (141). Produces populated PA-RQ- fields like CARD-NUM, AMT, MERCHANT-ID etc., converts alphanum amt to numeric via NUMVAL (163-166). No conditions or decisions. No error handling here. No calls.

### 3100-READ-REQUEST-MQ
Calls MQGET with wait options to read next request message from opened queue. Consumes W01-HCONN-REQUEST, HOBJ (182-186). Produces W01-GET-BUFFER, W01-DATALEN, saves CORRELID and REPLYTOQ on success (198-201). Logic sets options NO-SYNCPOINT+WAIT+CONVERT+FAIL-QUIESCING (176-178), on OK save ids, on NO-MSG set flag (203-204), else critical error log (206-217). No subordinate calls.

### 5000-PROCESS-AUTH
Core processing for one auth request: assumes approve initially, schedules IMS PSB, reads xref/acct/cust/authsum/profile (profile empty), makes decision, sends response MQ, conditionally writes to DB. Consumes PA-RQ-* from extract. Sets flags like CARD-FOUND-XREF (232), produces decision flags APPROVE-AUTH/DECLINE-AUTH, PA-RL-* response, updates IMS if found. Logic: if xref found, read chained records (237-244), always decide/send (246-248), write if found (250). Errors to log via reads. Calls 1200-SCHEDULE-PSB, 5100/5200/5300/5500/5600-READs, 6000-MAKE-DECISION, 7100-SEND-RESPONSE, 8000-WRITE-AUTH-TO-DB.

### 5100-READ-XREF-RECORD
CICS READ VSAM xref file by card num, sets found flags, logs warnings for NOTFND or critical for other resp. Consumes PA-RQ-CARD-NUM moved to XREF-CARD-NUM (262). Produces CARD-XREF-RECORD, CARD-FOUND-XREF or NFOUND (276,278). Logic EVALUATE RESP: NORMAL found, NOTFND warning 'A001' log, OTHER critical 'C001' (274-300). Calls 9500-LOG-ERROR on errors.

### 5200-READ-ACCT-RECORD
CICS READ VSAM acct file by acct ID from xref, sets found flags, logs warnings/NOTFND or critical errors. Consumes XREF-ACCT-ID to WS-CARD-RID-ACCT-ID (310). Produces ACCOUNT-RECORD data like limits/bal, FOUND-ACCT-IN-MSTR or NFOUND (324,326). EVALUATE RESP similar to 5100: NORMAL, NOTFND 'A002' warning, OTHER 'C002' critical (322-348). Calls 9500 on errors.

### 5300-READ-CUST-RECORD
CICS READ VSAM cust file by cust ID from xref, sets found flags, logs warnings/NOTFND or critical. Similar to 5200. Consumes XREF-CUST-ID (358). Produces CUSTOMER-RECORD, FOUND-CUST-IN-MSTR or NFOUND (372,374). EVALUATE: NOTFND 'A003', OTHER 'C003' (370-396). Calls 9500.

### 5500-READ-AUTH-SUMMRY
DLI GU IMS get unique for PAUTSUM0 summary segment by acct ID. Consumes PA-ACCT-ID (406). Produces PENDING-AUTH-SUMMARY, FOUND-PAUT-SMRY-SEG or NFOUND (416,418). EVALUATE DIBSTAT: OK found, SEG-NOT-FND nfound, OTHER critical 'I002' log (414-426). Calls 9500.

### 5600-READ-PROFILE-DATA
Empty paragraph, just CONTINUE; no action performed for profile data read. No inputs consumed or outputs produced. No logic, errors, or calls. Likely placeholder for future profile checks.

### 6000-MAKE-DECISION
Applies auth rules to set approve/decline, resp codes/reasons, builds response string buffer. Consumes PA-RQ-*, records from prior reads (credit limits/bals). Produces PA-RL-* response fields, W02-PUT-BUFFER string (516), WS-APPROVED-AMT. Logic: compute avail from summary or acct (452-470), decline if over or not found (468), set codes '00'/'05' (475,480), reason eval for declines (487-504), STRING response (509). No errors here.

### 7100-SEND-RESPONSE
Prepares and calls MQPUT1 to send response to reply queue using saved correlid. Consumes WS-REPLY-QNAME, WS-SAVE-CORRELID, W02-PUT-BUFFER (528-544). Produces message on queue if MQCC-OK. Sets MQMD reply fields (531-539), options NO-SYNCPOINT (540). On failure, critical MQ error 'M004' log (554-565). No subordinate calls.

### 8000-WRITE-AUTH-TO-DB
Updates IMS DB with auth summary and inserts detail record if xref found. No direct inputs beyond globals. Calls 8400 to update summary counts/bals, 8500 to insert detail with timestamps etc. Logic conditional on CARD-FOUND-XREF but always calls both? Wait, called only if found (250). Errors delegated.

### 8400-UPDATE-SUMMARY
Updates or inserts PAUTSUM0 summary: init if nfound, set limits from acct, inc counts/amts per approve/decline, REPL if found else ISRT. Consumes prior records/flags. Produces updated/inserted segment. Logic: if nfound init with ids (588-595), move limits (597), add to cnts/amts/bal (601-609), REPL/ISRT (611-620), check STATUS-OK else 'I003' critical (624-633). Calls 9500.

### 8500-INSERT-AUTH
Gets current CICS time/date, computes auth timestamps, populates PAUTH detail from request/response/xref, sets match flags, ISRT PAUTDTL1 under summary SSA. Consumes PA-RQ-*, PA-RL-*, XREF-IDs (644-683). Produces inserted detail segment. Logic: ASKTIME/FORMATTIME (644-653), compute inverted dates/times (661-662), move all fields (664-688), set MATCH-PENDING or DECLINED (689), ISRT with parent SSA (700), check OK else 'I004' (707-718). Calls 9500.

### 9000-TERMINATE
Cleanup: TERM IMS PSB if scheduled, close request MQ queue. Consumes flags IMS-PSB-SCHD, WS-REQUEST-MQ-OPEN. No outputs beyond side effects. Logic: conditional TERM (730-732), call close (734). Errors delegated to 9100.

### 9100-CLOSE-REQUEST-QUEUE
Calls MQCLOSE on request queue handle if open. Consumes W01-HOBJ-REQUEST (743). Sets WS-REQUEST-MQ-CLSE on OK (751), else warning MQ 'M005' log (753-762).

### 9500-LOG-ERROR
Formats and writes error log to CICS TD queue 'CSSL' with timestamp, tranid, pgm, err details. Consumes ERR-* fields set by callers, gets current time/date (773-781). Produces ERROR-LOG-RECORD written (790). If ERR-CRITICAL, performs 9990-END-ROUTINE (795-796). No conditions beyond critical check.

### 9990-END-ROUTINE
The 9990-END-ROUTINE paragraph serves as the primary termination handler for the COPAUA0C CICS program, ensuring orderly shutdown before returning control to CICS (lines 7-16). It consumes no explicit inputs or data within its scope, relying instead on the global program state, working storage variables, and any resources opened earlier in the execution flow. Its sole output is the implicit effects of cleanup actions and the termination of the transaction via CICS RETURN, with no files or variables written directly here. There is no business logic, conditions, or decisions implemented in this paragraph; it is a straightforward sequence of termination steps without validation or branching. Error handling is not performed within this paragraph; any errors would presumably be managed prior to reaching this point or within the called 9000-TERMINATE. It calls the 9000-TERMINATE paragraph (line 10) specifically to execute finalization tasks such as closing files, freeing CICS resources, or setting return codes, though the details of 9000-TERMINATE are not present in this chunk. Following the PERFORM, it executes the CICS RETURN (lines 12-13) to terminate the transaction and return control to the CICS region. Control then falls through to the 9990-EXIT label (line 15), which issues an EXIT statement (line 16) to return to the invoking paragraph, maintaining proper COBOL flow control.

### 9990-EXIT
The 9990-EXIT paragraph label and statement (lines 15-16) act as a flow control exit point at the end of 9990-END-ROUTINE, with no independent logic. It consumes no data and produces no outputs, serving solely to terminate the current paragraph's execution. No business logic, conditions, error handling, or calls are present. It is reached sequentially after the CICS RETURN in 9990-END-ROUTINE, allowing the program to exit cleanly back to its caller.

## Open Questions

- ? No PROCEDURE DIVISION provided in chunk 1/3
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Exact field names within copybooks unknown
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Initialization of queue names WS-REQUEST-QNAME and WS-REPLY-QNAME
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? File definitions (SELECT, FD, ASSIGN)
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Program invocation trigger
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Copybook and working storage definitions
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? 9990-END-ROUTINE and 5600 profile logic
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? Connection handles W01/02-HCONN and full MQ setup
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? Transaction ID and entry point
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? What is the overall business purpose of COPAUA0C?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
- ? What does the 9000-TERMINATE paragraph do?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
- ? What are the data divisions, files, copybooks, and main processing paragraphs?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
