# CBPAUP0C

**File**: `cbl/CBPAUP0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 14:20:57.096976

## Purpose

This batch IMS COBOL program deletes expired pending authorization detail segments from the PAUT database and removes summary segments when no details remain. It processes summary segments (PAUTSUM0) sequentially, retrieves child detail segments (PAUTDTL1), checks expiration based on authorization date and parameter-specified days, updates counters in the summary, deletes qualified details, and deletes empty summaries. Periodic checkpoints are taken, and statistics are displayed at completion.

**Business Context**: CardDemo Authorization Module: cleans up expired pending authorizations to maintain database efficiency

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PRM-INFO | IOType.PARAMETER | Command line parameters from SYSIN containing expiry days (P-EXPIRY-DAYS), checkpoint frequency (P-CHKP-FREQ), checkpoint display frequency (P-CHKP-DIS-FREQ), and debug flag (P-DEBUG-FLAG) |
| PAUT | IOType.IMS_SEGMENT | Pending Authorization Summary segments (PAUTSUM0 root) and Detail segments (PAUTDTL1 child) read via GN and GNP calls using PAUT-PCB |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUT | IOType.IMS_SEGMENT | Deletes expired Pending Authorization Detail segments (PAUTDTL1) and empty Summary segments (PAUTSUM0) via DLET calls; also console displays for statistics and debug |
| CONSOLE | IOType.REPORT | Displays processing statistics including total summaries/details read/deleted and checkpoint info |

## Business Rules

- **BR001**: Delete authorization detail if days elapsed since authorization date exceeds expiry days parameter
- **BR002**: Delete summary segment only if both approved and declined authorization counts are zero or less after processing details

## Paragraphs/Procedures

### MAIN-PARA
This is the primary orchestration paragraph controlling the overall program flow in this batch IMS cleanup job. It begins by performing 1000-INITIALIZE to accept parameters, set dates, and initialize variables from SYSIN and system dates. After initialization, it calls 2000-FIND-NEXT-AUTH-SUMMARY to start reading summary segments. It enters a main loop until end-of-database or error, processing each summary by repeatedly calling 3000-FIND-NEXT-AUTH-DTL and 4000-CHECK-IF-EXPIRED to evaluate details for expiration, deleting qualified details via 5000-DELETE-AUTH-DTL and updating summary counters. If summary counts reach zero, it calls 6000-DELETE-AUTH-SUMMARY. Periodically checks WS-AUTH-SMRY-PROC-CNT against P-CHKP-FREQ to invoke 9000-TAKE-CHECKPOINT for IMS checkpointing. Upon loop exit, takes final checkpoint, displays final statistics on reads/deletes to console, and GOBACKs. No explicit error handling beyond flags, but subordinate errors trigger ABEND via 9999-ABEND.

### 1000-INITIALIZE
This initialization paragraph sets up program variables and parameters for the cleanup process. It consumes system dates via ACCEPT CURRENT-DATE FROM DATE and CURRENT-YYDDD FROM DAY, and reads PRM-INFO from SYSIN containing expiry days, checkpoint frequencies, and debug flag. It validates and defaults P-EXPIRY-DAYS to 5 if non-numeric, P-CHKP-FREQ to 5, P-CHKP-DIS-FREQ to 10, and P-DEBUG-FLAG to 'N'. Outputs displays of start message, parameters, and date for logging. No files or segments read yet; sets WS-EXPIRY-DAYS and flags like ERR-FLG-OFF. No business decisions beyond parameter validation; no error handling beyond defaults. Does not call other paragraphs; exits to MAIN-PARA.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph retrieves the next Pending Authorization Summary root segment using IMS GN call on PAUTSUM0 into PENDING-AUTH-SUMMARY. It consumes the PAUT-PCB and prior position from IMS database. On success (DIBSTAT blank), sets NOT-END-OF-AUTHDB, increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves PA-ACCT-ID to WS-CURR-APP-ID. On 'GB' sets END-OF-AUTHDB. Other statuses display error and call 9999-ABEND. Debug display if enabled. No writes or transforms; error handling via EVALUATE on DIBSTAT leading to abend on failure. Called repeatedly from MAIN-PARA loop.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph retrieves the next child Pending Authorization Detail segment under current summary using IMS GNP on PAUTDTL1 into PENDING-AUTH-DETAILS. Consumes PAUT-PCB position from prior GN/GNP. On success (blank), sets MORE-AUTHS and increments WS-NO-DTL-READ. On 'GE' or 'GB' sets NO-MORE-AUTHS. Other statuses display details including PA-ACCT-ID and call 9999-ABEND. Debug display if enabled. No outputs or transforms; validates IMS status for continuation or end-of-children. Error handling abends on unexpected status.

### 4000-CHECK-IF-EXPIRED
This core business logic paragraph evaluates if a detail authorization is expired. It reads fields from PENDING-AUTH-DETAILS (PA-AUTH-DATE-9C, PA-AUTH-RESP-CODE, amounts) and working storage dates. Computes WS-AUTH-DATE = 99999 - PA-AUTH-DATE-9C and WS-DAY-DIFF = CURRENT-YYDDD - WS-AUTH-DATE. If WS-DAY-DIFF >= WS-EXPIRY-DAYS, sets QUALIFIED-FOR-DELETE and adjusts summary counters: for resp '00' decrement PA-APPROVED-AUTH-CNT/AMT, else PA-DECLINED-AUTH-CNT/AMT. Else sets NOT-QUALIFIED-FOR-DELETE. Modifies PENDING-AUTH-SUMMARY counters in memory. No I/O or calls; decisions based on date diff and resp code. No explicit errors.

### 5000-DELETE-AUTH-DTL
This paragraph performs the IMS DLET to remove a qualified expired detail segment. Consumes PENDING-AUTH-DETAILS and PAUT-PCB position. Executes DLI DLET on PAUTDTL1 FROM PENDING-AUTH-DETAILS. On success (blank DIBSTAT), increments WS-NO-DTL-DELETED. Else displays error with PA-ACCT-ID and abends via 9999-ABEND. Debug display of delete if enabled. Primary output is database deletion; error handling immediate abend on failure. Called conditionally from MAIN-PARA.

### 6000-DELETE-AUTH-SUMMARY
This paragraph deletes an empty summary segment after all details processed. Consumes PENDING-AUTH-SUMMARY and PAUT-PCB. Executes DLI DLET on PAUTSUM0 FROM PENDING-AUTH-SUMMARY. On success, increments WS-NO-SUMRY-DELETED. Else displays error with PA-ACCT-ID and abends. Debug display if enabled. Output is database deletion; strict error handling via abend. Called conditionally from MAIN-PARA when counts <=0.

### 9000-TAKE-CHECKPOINT
This utility paragraph issues IMS checkpoint for transaction integrity. Consumes WK-CHKPT-ID built from 'RMAD' + counter. Executes DLI CHKP ID(WK-CHKPT-ID). On success, increments WS-NO-CHKP and if >= P-CHKP-DIS-FREQ displays success with counts/APP-ID, resets counter. Else displays failure details and abends. No inputs beyond variables; outputs checkpoint to IMS and conditional display. Decisions on display frequency; error abend.

### 9999-ABEND
This error termination paragraph handles all abend conditions. No specific inputs; triggered by failures in IMS calls. Displays abend message, sets RETURN-CODE to 16, and GOBACKs. No business logic, validation, or calls; sole purpose is controlled program stop on error. Used throughout for IMS status failures.

## Open Questions

- ? Exact layout and all fields of copybooks CIPAUSMY and CIPAUDTY
  - Context: Referenced but contents not in source; fields like PA-ACCT-ID, PA-AUTH-DATE-9C used but full structure unknown
- ? Full list of possible PA-AUTH-RESP-CODE values and business meanings
  - Context: Code handles '00' as approved, others declined, but others unspecified
