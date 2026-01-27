# CBPAUP0C

**File**: `cbl/CBPAUP0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-27 02:44:51.177838

## Purpose

This batch IMS DL/I program processes pending authorization summary segments (PAUTSUM0) and their child detail segments (PAUTDTL1) in the PAUT database. For each detail segment, it calculates the age based on authorization date and deletes the detail if expired beyond the parameter-specified days threshold, adjusting summary counters in working storage accordingly. After processing all details for a summary, it deletes the summary segment if PA-APPROVED-AUTH-CNT <= 0 (condition coded with duplication of the same field check). Periodic checkpoints are taken, and final statistics are displayed.

**Business Context**: CardDemo Authorization Module: Batch cleanup of expired pending authorization messages from IMS database.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PRM-INFO | IOType.PARAMETER | Command line parameters from SYSIN containing expiry days, checkpoint frequency, checkpoint display frequency, and debug flag |
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segments for pending authorization summaries |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segments for pending authorization details under current summary |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTDTL1 | IOType.IMS_SEGMENT | Expired detail segments deleted from IMS database |
| PAUTSUM0 | IOType.IMS_SEGMENT | Summary segments with no remaining approved auth count deleted from IMS database |
| Console | IOType.REPORT | Processing statistics including counts of summaries/details read/deleted and checkpoints |

## Business Rules

- **BR001**: Pending authorization detail qualifies for deletion if days elapsed since authorization date (computed as CURRENT-YYDDD - (99999 - PA-AUTH-DATE-9C)) is greater than or equal to expiry days parameter
- **BR002**: Upon qualifying a detail for deletion, adjust summary segment counters in working storage: for approved (resp code '00') decrement count and approved amount; for declined decrement count and transaction amount
- **BR003**: Delete summary segment only if PA-APPROVED-AUTH-CNT <= 0 (condition implemented as PA-APPROVED-AUTH-CNT <= 0 AND PA-APPROVED-AUTH-CNT <= 0)

## Paragraphs/Procedures

### MAIN-PARA
This is the primary orchestration paragraph controlling the entire program flow for deleting expired authorizations. It begins by performing 1000-INITIALIZE to accept parameters, dates, and set defaults. It then enters the main loop by calling 2000-FIND-NEXT-AUTH-SUMMARY to retrieve the next summary segment until end of database or error. For each summary, it loops through 3000-FIND-NEXT-AUTH-DTL to get child details, calling 4000-CHECK-IF-EXPIRED to assess expiry and adjust summary counters in memory if qualified, followed by 5000-DELETE-AUTH-DTL if applicable. After all details, it checks if PA-APPROVED-AUTH-CNT <= 0 (duplicated condition) and performs 6000-DELETE-AUTH-SUMMARY if true. Periodically takes checkpoints via 9000-TAKE-CHECKPOINT when processing count exceeds frequency. Upon loop exit, performs final checkpoint, displays total statistics (reads/deletes for summary/details), and returns. Errors from subordinate paragraphs trigger abend via 9999-ABEND indirectly through status checks. No direct validation beyond subordinate returns.

### 1000-INITIALIZE
This initialization paragraph sets up program variables and parameters for processing. It accepts CURRENT-DATE from DATE and CURRENT-YYDDD from DAY for expiry calculations. It reads PRM-INFO from SYSIN and displays program start, parameters, and date. It validates and moves P-EXPIRY-DAYS to WS-EXPIRY-DAYS if numeric, else defaults to 5. It defaults P-CHKP-FREQ to 5 and P-CHKP-DIS-FREQ to 10 if invalid, and ensures P-DEBUG-FLAG is 'Y' or 'N'. Displays initial information for audit trail. No file opens or IMS calls; relies on batch IMS environment. No explicit error handling beyond parameter defaults; proceeds with defaults on invalid input. Does not call other paragraphs.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph retrieves the next pending authorization summary root segment from IMS database. If debug on, displays current summary read count. Performs DL/I GN call on PAUT-PCB for PAUTSUM0 segment into PENDING-AUTH-SUMMARY. Evaluates DIBSTAT: if blank, sets not end-of-DB, increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves PA-ACCT-ID to WS-CURR-APP-ID. If 'GB', sets END-OF-AUTHDB true. Any other status displays error details and performs 9999-ABEND. Serves as entry to each summary processing iteration. Inputs from IMS PCB; outputs updated counters and current app ID in WS. No subordinate calls.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph retrieves the next child detail segment under the current summary from IMS database. If debug on, displays current detail read count. Performs DL/I GNP call on PAUT-PCB for PAUTDTL1 segment into PENDING-AUTH-DETAILS. Evaluates DIBSTAT: if blank, sets MORE-AUTHS true and increments WS-NO-DTL-READ. If 'GE' (not found) or 'GB' (end DB), sets NO-MORE-AUTHS true. Any other status displays error with app ID and read count, then abends via 9999-ABEND. Controls the inner loop for detail processing per summary. Inputs current position from prior GNP; outputs populated detail record or end flag.

### 4000-CHECK-IF-EXPIRED
This paragraph determines if the current detail segment is expired and prepares summary for adjustment if so. Computes WS-AUTH-DATE by subtracting PA-AUTH-DATE-9C from 99999 to normalize date. Computes WS-DAY-DIFF as CURRENT-YYDDD minus WS-AUTH-DATE. If WS-DAY-DIFF >= WS-EXPIRY-DAYS, sets QUALIFIED-FOR-DELETE true and adjusts summary counters: if PA-AUTH-RESP-CODE='00', decrements PA-APPROVED-AUTH-CNT and subtracts PA-APPROVED-AMT from PA-APPROVED-AUTH-AMT; else decrements PA-DECLINED-AUTH-CNT and subtracts PA-TRANSACTION-AMT from PA-DECLINED-AUTH-AMT. If not expired, sets NOT-QUALIFIED-FOR-DELETE. All adjustments in working storage memory only. Implements core business logic for expiry. No I/O or calls; validates date fields implicitly via computation.

### 5000-DELETE-AUTH-DTL
This paragraph deletes the current qualified expired detail segment from IMS database. If debug on, displays app ID for audit. Performs DL/I DLET call on PAUT-PCB for PAUTDTL1 from PENDING-AUTH-DETAILS. If DIBSTAT blank, increments WS-NO-DTL-DELETED. If not blank, displays failure status and app ID, then abends via 9999-ABEND. Executed conditionally after expiry check. Inputs populated detail record; outputs deletion and updated delete counter. Error handling via status check leading to abend. No subordinate calls.

### 6000-DELETE-AUTH-SUMMARY
This paragraph deletes the current summary segment when no approved auths remain. If debug on, displays app ID. Performs DL/I DLET call on PAUT-PCB for PAUTSUM0 from PENDING-AUTH-SUMMARY. If DIBSTAT blank, increments WS-NO-SUMRY-DELETED. If not blank, displays failure status and app ID, then abends via 9999-ABEND. Called conditionally after all details processed. Inputs adjusted summary record; outputs deletion and counter update. Strict error handling with abend on failure.

### 9000-TAKE-CHECKPOINT
This paragraph commits current IMS changes via checkpoint for restartability. Performs DL/I CHKP with WK-CHKPT-ID (RMADnnnn). If DIBSTAT blank, increments WS-NO-CHKP; if reaches P-CHKP-DIS-FREQ, resets counter and displays success with read count and app ID. If not blank, displays failure details and abends via 9999-ABEND. Called periodically in main loop and at end. Ensures data integrity in long-running batch. Inputs current checkpoint ID; outputs committed position or error.

### 9999-ABEND
This terminal error handling paragraph abends the program on any IMS or processing failure. Displays 'CBPAUP0C ABENDING ...' message. Sets RETURN-CODE to 16 and GOBACKs. Called from status evaluation points in IMS calls. No recovery or retry; immediate termination. Inputs error context from caller displays; outputs non-zero return code. Minimal logic focused on exit.

## Open Questions

- ? Does the program persist adjusted counts to summary segments when not deleting the summary?
  - Context: Counts like PA-APPROVED-AUTH-CNT are modified in working storage after detail deletions (lines 288-292), but no IMS CHG or ISRT call updates the database segment unless fully deleted (line 335).
- ? Is the duplicated condition in line 156 intended to check PA-DECLINED-AUTH-CNT instead of PA-APPROVED-AUTH-CNT twice?
  - Context: Logic checks PA-APPROVED-AUTH-CNT <= 0 AND PA-APPROVED-AUTH-CNT <= 0, ignoring declined counts despite adjustments to both.
