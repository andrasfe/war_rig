# CBPAUP0C

**File**: `cbl/CBPAUP0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 15:14:25.753983

## Purpose

This IMS batch COBOL program deletes expired pending authorization detail segments (PAUTDTL1) from the PAUT database under PSB PSBPAUTB. For each summary segment (PAUTSUM0), it reads child details, computes age from auth date against configurable expiry days, deletes expired details, adjusts summary counters in working storage memory, and conditionally deletes the summary segment if PA-APPROVED-AUTH-CNT <= 0 (redundantly checked). It takes periodic IMS checkpoints and displays processing statistics upon completion.

**Business Context**: CardDemo Authorization Module: Automated cleanup of expired pending authorization messages to maintain database hygiene.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PRM-INFO | IOType.PARAMETER | Command-line parameters from SYSIN containing expiry days (P-EXPIRY-DAYS), checkpoint frequency (P-CHKP-FREQ), checkpoint display frequency (P-CHKP-DIS-FREQ), and debug flag (P-DEBUG-FLAG) |
| PAUTSUM0 | IOType.IMS_SEGMENT | Pending Authorization Summary root segments read sequentially via GN call |
| PAUTDTL1 | IOType.IMS_SEGMENT | Pending Authorization Details child segments read via GNP call under current summary |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTDTL1 | IOType.IMS_SEGMENT | Expired Pending Authorization Details segments deleted from IMS database via DLET |
| PAUTSUM0 | IOType.IMS_SEGMENT | Pending Authorization Summary segments with no remaining approved auth count deleted from IMS database via DLET |
| STATS-REPORT | IOType.REPORT | Console display of processing statistics including summaries read/deleted and details read/deleted |
| RETURN-CODE | IOType.RETURN_CODE | Set to 16 upon abend conditions |

## Business Rules

- **BR001**: Pending authorization detail qualifies for deletion if the number of days since authorization date exceeds or equals the configured expiry days
- **BR002**: On qualified deletion, decrement appropriate summary counters based on response code: approved if '00' else declined
- **BR003**: Delete summary segment if PA-APPROVED-AUTH-CNT <= 0 (condition checked redundantly against itself)
- **BR004**: Take IMS checkpoint after every P-CHKP-FREQ summary records processed

## Paragraphs/Procedures

### MAIN-PARA
This is the primary orchestration paragraph controlling the entire program flow from initialization to termination. It consumes IMS PCB masks from linkage and begins by performing 1000-INITIALIZE to accept current date, read parameters from SYSIN, and set defaults for expiry days, checkpoint frequencies, and debug flag. It then enters an outer loop performing 2000-FIND-NEXT-AUTH-SUMMARY until end-of-DB or error, reading summary segments into PENDING-AUTH-SUMMARY. For each summary, it enters an inner loop with 3000-FIND-NEXT-AUTH-DTL to read child details into PENDING-AUTH-DETAILS until no more. For each detail, it performs 4000-CHECK-IF-EXPIRED to compute age and qualify for delete, adjusting summary counters in memory if expired, and conditionally performs 5000-DELETE-AUTH-DTL. After inner loop, it checks if PA-APPROVED-AUTH-CNT <= 0 (twice) and performs 6000-DELETE-AUTH-SUMMARY if true. Periodically checks WS-AUTH-SMRY-PROC-CNT against P-CHKP-FREQ to perform 9000-TAKE-CHECKPOINT. Upon outer loop exit, takes final checkpoint, displays aggregate statistics (reads/deletes for summary/details), and GOBACKs. Errors from subordinate paragraphs propagate via WS-ERR-FLG or abend calls. No direct validation beyond parameter numeric check; relies on IMS status evaluation in callees.

### 1000-INITIALIZE
This initialization paragraph sets up program variables and parameters before main processing. It consumes system date via ACCEPT CURRENT-DATE FROM DATE and CURRENT-YYDDD FROM DAY into working storage. Reads PRM-INFO from SYSIN into parameter area, displays start message, parameters, and date. Validates and sets WS-EXPIRY-DAYS from P-EXPIRY-DAYS if numeric else defaults to 5; sets P-CHKP-FREQ to 5 if invalid; P-CHKP-DIS-FREQ to 10 if invalid; P-DEBUG-FLAG to 'N' if not 'Y'. Produces initialized WS-VARIABLES, PRM-INFO, and console display messages. Implements business logic for parameter defaults and validation. No error handling beyond numeric check; invalid params use defaults without abend. Called only from MAIN-PARA at start.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph retrieves the next Pending Authorization Summary root segment via IMS GN call. It conditionally displays debug read count if DEBUG-ON. Performs EXEC DLI GN on PAUT-PCB-NUM for SEGMENT PAUTSUM0 into PENDING-AUTH-SUMMARY. Evaluates DIBSTAT: if blank, sets NOT-END-OF-AUTHDB, increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves PA-ACCT-ID to WS-CURR-APP-ID; if 'GB' sets END-OF-AUTHDB; other statuses display error and perform 9999-ABEND. Consumes IMS database via PCB; produces loaded PENDING-AUTH-SUMMARY or EOF flag. Implements decision on IMS status for continue/abend. Error handling abends on unexpected status. Called repeatedly from MAIN-PARA outer loop.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph retrieves the next Pending Authorization Details child segment under current summary via IMS GNP call. Conditionally displays debug read count if DEBUG-ON. Performs EXEC DLI GNP on PAUT-PCB-NUM for SEGMENT PAUTDTL1 into PENDING-AUTH-DETAILS. Evaluates DIBSTAT: if blank sets MORE-AUTHS and increments WS-NO-DTL-READ; if 'GE' or 'GB' sets NO-MORE-AUTHS; other displays error with current PA-ACCT-ID and performs 9999-ABEND. Consumes IMS database via PCB under current parent; produces loaded PENDING-AUTH-DETAILS or no-more flag. Handles segment-not-found and end-of-DB as normal termination of inner loop. Error handling abends on unexpected status. Called repeatedly from MAIN-PARA inner loop.

### 4000-CHECK-IF-EXPIRED
This paragraph determines if current detail segment is expired and adjusts summary counters if so. Consumes fields from PENDING-AUTH-DETAILS (PA-AUTH-DATE-9C, PA-AUTH-RESP-CODE) and working storage CURRENT-YYDDD/WS-EXPIRY-DAYS; also modifies fields in PENDING-AUTH-SUMMARY counters. Computes WS-AUTH-DATE as 99999 - PA-AUTH-DATE-9C then WS-DAY-DIFF as CURRENT-YYDDD - WS-AUTH-DATE. If WS-DAY-DIFF >= WS-EXPIRY-DAYS sets QUALIFIED-FOR-DELETE and adjusts counters: if PA-AUTH-RESP-CODE='00' decrement/subtract from approved cnt/amt else from declined; else sets NOT-QUALIFIED-FOR-DELETE. Implements core business logic for expiration check and counter maintenance (memory-only). No error handling or validation; assumes valid data. Called from MAIN-PARA for each detail.

### 5000-DELETE-AUTH-DTL
This paragraph deletes the current qualified expired detail segment from IMS. Conditionally displays debug delete message with PA-ACCT-ID if DEBUG-ON. Performs EXEC DLI DLET on PAUT-PCB-NUM for SEGMENT PAUTDTL1 FROM PENDING-AUTH-DETAILS. If DIBSTAT blank, increments WS-NO-DTL-DELETED; else displays error with PA-ACCT-ID and performs 9999-ABEND. Consumes loaded PENDING-AUTH-DETAILS; produces database deletion and updated delete counter. Business logic assumes prior qualification; no re-check. Error handling abends on failed delete. Called conditionally from MAIN-PARA after expiry check.

### 6000-DELETE-AUTH-SUMMARY
This paragraph deletes the current summary segment if no remaining details. Conditionally displays debug delete message with PA-ACCT-ID if DEBUG-ON. Performs EXEC DLI DLET on PAUT-PCB-NUM for SEGMENT PAUTSUM0 FROM PENDING-AUTH-SUMMARY. If DIBSTAT blank, increments WS-NO-SUMRY-DELETED; else displays error with PA-ACCT-ID and performs 9999-ABEND. Consumes loaded PENDING-AUTH-SUMMARY; produces database deletion and updated delete counter. Called only after all details processed and condition met in MAIN-PARA. Error handling abends on failed delete.

### 9000-TAKE-CHECKPOINT
This paragraph performs an IMS checkpoint to commit changes periodically. Performs EXEC DLI CHKP ID(WK-CHKPT-ID). If DIBSTAT blank, increments WS-NO-CHKP and if >= P-CHKP-DIS-FREQ resets counter and displays success with counts/APP-ID; else displays failure details and performs 9999-ABEND. Consumes checkpoint ID and counters; produces committed DB changes and optional display. Business logic for restartability via checkpoints every configured summaries. Error handling abends on failed CHKP. Called from MAIN-PARA periodically and at end.

### 9999-ABEND
This terminal error handler abends the program on IMS failures or other irrecoverable conditions. Displays abend message 'CBPAUP0C ABENDING ...'. Sets RETURN-CODE to 16 and GOBACKs, causing batch job abend. Consumes no inputs; produces non-zero return code for job control. No business logic; purely error termination. Called from multiple paragraphs on IMS status errors. No further error handling.

## Open Questions

- ? Are summary segment counters (PA-APPROVED-AUTH-CNT etc.) intended to be updated in the database for non-deleted summaries?
  - Context: Counters modified in memory at lines 288-292 but no IMS REPL call; changes lost unless summary deleted via DLET.
- ? Is the summary delete condition at line 156 a defect (checks PA-APPROVED-AUTH-CNT <=0 twice)?
  - Context: Code: PA-APPROVED-AUTH-CNT <= 0 AND PA-APPROVED-AUTH-CNT <= 0; likely meant to check both approved and declined counts.
