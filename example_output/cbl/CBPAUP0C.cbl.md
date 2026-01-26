# CBPAUP0C

**File**: `cbl/CBPAUP0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 02:35:11.996148

## Purpose

This batch IMS COBOL program deletes expired pending authorization detail segments from the IMS database and removes summary segments that have no remaining approved or declined authorizations after processing. It reads summary segments (PAUTSUM0), processes child detail segments (PAUTDTL1) by checking age against an expiry threshold from parameters, deletes expired details while decrementing in-memory summary counters, and deletes empty summaries. Periodic checkpoints are taken, and final statistics are displayed.

**Business Context**: CardDemo Authorization Module: Cleanup of expired pending authorization messages to maintain database hygiene.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PRM-INFO | IOType.PARAMETER | Program parameters including expiry days (P-EXPIRY-DAYS), checkpoint frequency (P-CHKP-FREQ), display frequency (P-CHKP-DIS-FREQ), and debug flag (P-DEBUG-FLAG) |
| PENDING-AUTH-SUMMARY | IOType.IMS_SEGMENT | Root segment (PAUTSUM0) containing account ID, approved/declined auth counts and amounts |
| PENDING-AUTH-DETAILS | IOType.IMS_SEGMENT | Child segment (PAUTDTL1) containing authorization date, response code, transaction amount |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PENDING-AUTH-DETAILS | IOType.IMS_SEGMENT | Expired detail segments deleted from IMS database |
| PENDING-AUTH-SUMMARY | IOType.IMS_SEGMENT | Empty summary segments (no remaining auths) deleted from IMS database |
| STATS-DISPLAY | IOType.REPORT | Console display of total summaries/details read and deleted |
| RETURN-CODE | IOType.RETURN_CODE | Set to 16 on abend |

## Business Rules

- **BR001**: Delete authorization detail if days since authorization date exceeds expiry days threshold
- **BR002**: Decrement appropriate summary counters when detail qualifies for deletion: approved if PA-AUTH-RESP-CODE = '00', else declined
- **BR003**: Delete summary segment if PA-APPROVED-AUTH-CNT <= 0 AND PA-APPROVED-AUTH-CNT <= 0 (note: apparent code error duplicating approved count instead of declined)

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph controlling the entire program flow for deleting expired authorizations. It consumes no direct inputs but relies on initialized WS variables and parameters from 1000-INITIALIZE. It first calls 1000-INITIALIZE to set up dates, parameters, and flags, producing initialized WS-VARIABLES, PRM-INFO defaults, and CURRENT-YYDDD. Then it initiates the main processing loop by calling 2000-FIND-NEXT-AUTH-SUMMARY to position on the first summary segment. The loop continues until END-OF-AUTHDB or ERR-FLG-ON (though ERR-FLG-ON is never set in code), processing each summary's details via nested loops of 3000-FIND-NEXT-AUTH-DTL and 4000-CHECK-IF-EXPIRED, deleting qualified details with 5000-DELETE-AUTH-DTL and adjusting in-memory summary counters. Business logic decisions include checking if summary counts are <=0 after detail processing to trigger 6000-DELETE-AUTH-SUMMARY, and taking checkpoints via 9000-TAKE-CHECKPOINT every P-CHKP-FREQ summaries. Error handling defers to called paragraphs' IMS status checks which abend on failure. After the loop, it performs a final checkpoint, displays aggregate statistics on reads/deletes, and GOBACKs. No direct calls to external programs.

### 1000-INITIALIZE
This initialization paragraph sets up program variables, accepts parameters, and applies defaults before main processing. It reads CURRENT-DATE/YYDDD from system and PRM-INFO from SYSIN, producing populated WS-VARIABLES (e.g., WS-EXPIRY-DAYS, P-CHKP-FREQ) and debug settings. No IMS access here. Business logic validates P-EXPIRY-DAYS numeric (default 5), sets checkpoint frequencies to 5/10 if invalid, and forces P-DEBUG-FLAG to 'N' if not 'Y'. Displays startup info including parameters and date. No error handling beyond parameter defaults; invalid params do not abend. Calls no other paragraphs. Exits cleanly to MAIN-PARA.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph positions on and reads the next PENDING-AUTH-SUMMARY root segment using IMS GN call. It consumes the current PCB position and produces data in PENDING-AUTH-SUMMARY (e.g., PA-ACCT-ID into WS-CURR-APP-ID) and increments WS-NO-SUMRY-READ/WS-AUTH-SMRY-PROC-CNT. Business logic via EVALUATE DIBSTAT: ' ' sets NOT-END-OF-AUTHDB and increments counters; 'GB' sets END-OF-AUTHDB; other statuses display error and abend via 9999-ABEND. Debug display if enabled. Error handling abends on unexpected IMS status. Called repeatedly from MAIN-PARA loop.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph reads the next child PENDING-AUTH-DETAILS segment under the current summary using IMS GNP. It consumes the parent position and produces data in PENDING-AUTH-DETAILS, setting MORE-AUTHS or NO-MORE-AUTHS based on status, incrementing WS-NO-DTL-READ on success. Business logic EVALUATE DIBSTAT: ' ' sets MORE-AUTHS; 'GE'/'GB' sets NO-MORE-AUTHS; other displays details (DIBSTAT, PA-ACCT-ID, count) and abends. Debug display if enabled. Error handling abends on failure. Called in nested loop from MAIN-PARA.

### 4000-CHECK-IF-EXPIRED
This paragraph determines if the current detail is expired by computing age and qualifying for deletion. It reads fields from PENDING-AUTH-DETAILS (PA-AUTH-DATE-9C, PA-AUTH-RESP-CODE, amounts) and CURRENT-YYDDD, producing WS-AUTH-DATE, WS-DAY-DIFF, and QUALIFIED-FOR-DELETE flag. If qualified (>= WS-EXPIRY-DAYS), decrements in-memory summary counters/amounts based on response code ('00' approved else declined). Business logic implements expiry check and counter adjustment logic. No I/O or error handling here; relies on prior read success. No calls. Sets NOT-QUALIFIED-FOR-DELETE otherwise.

### 5000-DELETE-AUTH-DTL
This paragraph deletes the current qualified expired PENDING-AUTH-DETAILS segment using IMS DLET. It consumes the positioned detail in PENDING-AUTH-DETAILS and PCB, producing increment to WS-NO-DTL-DELETED on success. Business logic checks DIBSTAT = SPACES for success; else displays (DIBSTAT, PA-ACCT-ID) and abends. Debug display if enabled. Error handling abends on delete failure. Called conditionally from MAIN-PARA after expiry check.

### 6000-DELETE-AUTH-SUMMARY
This paragraph deletes the current empty PENDING-AUTH-SUMMARY segment using IMS DLET. It consumes the positioned summary in PENDING-AUTH-SUMMARY and PCB (with potentially updated in-memory counters), producing increment to WS-NO-SUMRY-DELETED on success. Business logic checks DIBSTAT = SPACES; else displays (DIBSTAT, PA-ACCT-ID) and abends. Debug display if enabled. Error handling abends on failure. Called conditionally from MAIN-PARA if counts <=0.

### 9000-TAKE-CHECKPOINT
This paragraph issues an IMS checkpoint using CHKP with WK-CHKPT-ID. It consumes current position/counts, producing increment to WS-NO-CHKP and display every P-CHKP-DIS-FREQ checkpoints including read count and app ID. Business logic checks DIBSTAT = SPACES for success; else displays failure details and abends. Error handling abends on CHKP failure. Called periodically from MAIN-PARA and at end.

### 9999-ABEND
This terminal error handler abends the program on IMS failures or other issues. It consumes no inputs, produces DISPLAY of abend message and sets RETURN-CODE to 16 before GOBACK. No business logic, decisions, or validations. No error handling (terminal). Called from multiple paragraphs on IMS status errors.

## Open Questions

- ? Are summary counters (PA-APPROVED-AUTH-CNT etc.) intended to be persisted via REPL after detail deletions, even if summary not deleted?
  - Context: Code decrements counters in memory (lines 288-293) but performs no IMS REPL on summary; changes lost on next GN (165) unless DLET (335). Likely bug as counters would not reflect deleted details.
- ? Is line 156 condition bugged? IF PA-APPROVED-AUTH-CNT <= 0 AND PA-APPROVED-AUTH-CNT <= 0 (duplicate field)
  - Context: Probably meant PA-DECLINED-AUTH-CNT for second check to delete only truly empty summaries.
