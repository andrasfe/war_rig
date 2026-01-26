# CBPAUP0C

**File**: `cbl/CBPAUP0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 17:41:56.916129

## Purpose

This batch IMS COBOL program deletes expired pending authorization detail segments from the PAUT database and conditionally deletes their parent summary segments. It sequentially reads root summary segments (PAUTSUM0), then child detail segments (PAUTDTL1), checks each detail's age against a configurable expiry threshold, deletes qualified details while decrementing summary counters in memory, and deletes summaries with zero remaining counts. Checkpoints are taken periodically, and statistics are displayed at completion.

**Business Context**: CardDemo Authorization Module - purges expired pending authorization messages to maintain database hygiene

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PRM-INFO | IOType.PARAMETER | Command-line parameters from SYSIN containing expiry days (P-EXPIRY-DAYS), checkpoint frequency (P-CHKP-FREQ), display frequency (P-CHKP-DIS-FREQ), and debug flag (P-DEBUG-FLAG) |
| PENDING-AUTH-SUMMARY | IOType.IMS_SEGMENT | Root segment (PAUTSUM0) containing account ID, approved/declined auth counts and amounts from IMS database via PSBPAUTB PCB |
| PENDING-AUTH-DETAILS | IOType.IMS_SEGMENT | Child segment (PAUTDTL1) under current summary containing authorization date, response code, transaction amount from IMS database via PSBPAUTB PCB |
| IO-PCB-MASK | IOType.IMS_SEGMENT | IMS I/O PCB mask passed via linkage for database access |
| PGM-PCB-MASK | IOType.IMS_SEGMENT | IMS program PCB mask passed via linkage for database access |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PENDING-AUTH-DETAILS | IOType.IMS_SEGMENT | Expired detail segments deleted from IMS database |
| PENDING-AUTH-SUMMARY | IOType.IMS_SEGMENT | Empty summary segments (zero approved/declined counts) deleted from IMS database |
| WK-CHKPT-ID | IOType.OTHER | IMS checkpoints taken periodically using checkpoint ID |
| Program Statistics | IOType.REPORT | Console displays of summary/detail read/deleted counts and checkpoints |

## Business Rules

- **BR001**: Delete a pending authorization detail segment if the number of days since its authorization date exceeds the configured expiry days
- **BR002**: When qualifying a detail for deletion, decrement the appropriate summary segment counters in memory: approved count/amount if PA-AUTH-RESP-CODE = '00', else declined count/amount
- **BR003**: Delete the parent summary segment if both approved and declined authorization counts are <= 0 after processing details (note: code checks PA-APPROVED-AUTH-CNT twice, likely a bug intending PA-DECLINED-AUTH-CNT)
- **BR004**: Take IMS checkpoint after every P-CHKP-FREQ summaries processed (default 5) or at end

## Paragraphs/Procedures

### MAIN-PARA
This is the primary orchestration paragraph controlling the entire program execution flow in this batch IMS job. It first performs 1000-INITIALIZE to accept parameters from SYSIN, compute current date in YYDDD, set defaults for expiry days (default 5), checkpoint frequencies (5/10), and debug flag. It then initiates reading of summary segments via repeated 2000-FIND-NEXT-AUTH-SUMMARY calls until end-of-DB or error flag. For each summary (identified by PA-ACCT-ID), it enters an inner loop calling 3000-FIND-NEXT-AUTH-DTL to read child details until no more, applying 4000-CHECK-IF-EXPIRED to compute age and qualify deletions, executing 5000-DELETE-AUTH-DTL for qualified details while decrementing summary counters in memory (approved/declined based on response code). After processing all details for a summary, it checks if both counts <=0 (bug: duplicate approved check) and performs 6000-DELETE-AUTH-SUMMARY if true. Periodically takes 9000-TAKE-CHECKPOINT if summaries processed exceed frequency. At EOF, final checkpoint and displays total read/deleted stats to console. Errors in subordinates trigger abend via 9999-ABEND; no local error handling beyond loop exits on ERR-FLG-ON.

### 1000-INITIALIZE
This initialization paragraph sets up all working storage variables, dates, and parameters essential for program operation. It consumes system date via ACCEPT CURRENT-DATE FROM DATE and CURRENT-YYDDD FROM DAY (lines 186-187), and reads PRM-INFO from SYSIN (189). It produces initialized WS-EXPIRY-DAYS (from P-EXPIRY-DAYS or default 5, 196-200), defaults P-CHKP-FREQ to 5 (201-203), P-CHKP-DIS-FREQ to 10 (204-206), P-DEBUG-FLAG to 'N' (207-209). Business logic includes numeric validation for P-EXPIRY-DAYS and space/zero/low-values checks for frequencies with defaults. Displays startup info including parms and date (190-194). No subordinate calls or error handling beyond defaults; assumes SYSIN present. Exits cleanly to MAIN-PARA.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph reads the next root pending auth summary segment from IMS DB using GN call on PAUTSUM0. It consumes IMS PCB at PAUT-PCB-NUM (223-226) and current DB position. Produces loaded PENDING-AUTH-SUMMARY data, increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT (231-232), sets WS-CURR-APP-ID = PA-ACCT-ID (233), sets NOT-END-OF-AUTHDB (230). Business logic evaluates DIBSTAT: ' ' continues, 'GB' sets END-OF-AUTHDB (235), other statuses display error and abend via 9999-ABEND (237-240). Optional debug display (219-220). Error handling abends on non-ok/non-EOF status. No calls.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph retrieves the next child pending auth detail segment under the current summary using GNP on PAUTDTL1. Consumes IMS PCB and parent position from prior summary read (255-258). Produces loaded PENDING-AUTH-DETAILS data, sets MORE-AUTHS and increments WS-NO-DTL-READ (261-262). Business logic via DIBSTAT: ' ' continues loop, 'GE'/'GB' sets NO-MORE-AUTHS (264-266), other displays details (app ID, read count) and abends (267-270). Optional debug (251-252). Error handling abends on unexpected status. No calls.

### 4000-CHECK-IF-EXPIRED
This paragraph determines if current detail is expired by computing its age against expiry threshold. Consumes PA-AUTH-DATE-9C from detail, CURRENT-YYDDD, WS-EXPIRY-DAYS. Produces WS-AUTH-DATE (280), WS-DAY-DIFF (282), sets QUALIFIED-FOR-DELETE if >= threshold (284-285), and if qualified, decrements summary counters conditionally on PA-AUTH-RESP-CODE='00' (approved: 288-289) else declined (291-292). Business logic implements expiry check and counter adjustment in memory only (non-persistent). No writes or displays. No error handling or calls; sets flags for caller.

### 5000-DELETE-AUTH-DTL
This paragraph executes IMS DLET to remove qualified expired detail segment. Consumes current PENDING-AUTH-DETAILS in memory and PCB position (310-313). Produces deletion in DB, increments WS-NO-DTL-DELETED if success (316). Business logic checks DIBSTAT=spaces for success, else displays app ID and abends (318-320). Optional debug display of app ID (307). Error handling abends on failure. No further calls.

### 6000-DELETE-AUTH-SUMMARY
This paragraph deletes the current summary segment if no remaining auths after detail processing. Consumes PENDING-AUTH-SUMMARY in memory (with adjusted counters) and PCB (335-338). Produces DB deletion, increments WS-NO-SUMRY-DELETED if DIBSTAT=spaces (341). Business logic abends on failure with display of status and app ID (343-345). Optional debug (332). Error handling via abend.

### 9000-TAKE-CHECKPOINT
This paragraph issues IMS CHKP to commit changes periodically or at end. Consumes WK-CHKPT-ID (355). Produces checkpoint in IMS, increments WS-NO-CHKP (359), displays success message with counts/app ID every P-CHKP-DIS-FREQ (361-364). Business logic checks DIBSTAT=spaces for success, else displays failure details and abends (366-369). No data reads. Error handling abends on failure.

### 9999-ABEND
Universal error termination paragraph invoked on any IMS failure or invalid status. Consumes no inputs beyond caller context. Produces display 'CBPAUP0C ABENDING ...' (380), sets RETURN-CODE=16 (382), and GOBACKs. No business logic, decisions, validations, or calls. Handles all errors by forced abend.

## Open Questions

- ? Exact field layouts in copybooks CIPAUSMY and CIPAUDTY
  - Context: Copybooks referenced but contents not provided; fields like PA-AUTH-DATE-9C inferred from usage
- ? Apparent bug in line 156: checks PA-APPROVED-AUTH-CNT <=0 twice instead of approved AND declined
  - Context: Leads to deleting summary only if approved <=0, ignoring declined count
- ? Persistence of summary counter decrements when summary not deleted
  - Context: Memory changes in 288-292 not written back via IMS REPL/CHG/ISRT; counters desynchronized if non-expired details remain
