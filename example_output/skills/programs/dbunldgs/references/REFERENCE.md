# DBUNLDGS - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** DBUNLDGS
- **File Name:** cbl/DBUNLDGS.CBL
- **File Type:** COBOL
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:47:20.866610

## Purpose

**Summary:** DBUNLDGS is an IMS batch utility that browses and unloads root Pending Authorization Summary segments (PAUTSUM0) and their child Pending Authorization Detail segments (PAUTDTL1) from the PAUT IMS database using unqualified SSAs via the PAUTBPCB. For each root segment retrieved via GN call, it inserts a copy into a GSAM database using PASFLPCB and then retrieves all children via GNP calls, inserting each into another GSAM via PADFLPCB. It continues until end of database (GB status), increments counters, and abends on errors.

**Business Context:** Supports unloading of pending authorization data from IMS for potential archiving, migration, or reporting in a financial authorization system.
**Program Type:** BATCH

## Inputs

### PAUT IMS Database

- **Type:** IMS_SEGMENT
- **Description:** Root PAUTSUM0 summary segments and child PAUTDTL1 detail segments accessed via PAUTBPCB browse PCB
- **Copybook:** CIPAUSMY, CIPAUDTY

## Outputs

### PASFL GSAM

- **Type:** IMS_SEGMENT
- **Description:** Copies of unloaded PAUTSUM0 root summary segments inserted via PASFLPCB
- **Copybook:** CIPAUSMY

### PADFL GSAM

- **Type:** IMS_SEGMENT
- **Description:** Copies of unloaded PAUTDTL1 child detail segments inserted via PADFLPCB
- **Copybook:** CIPAUDTY

## Business Rules

### BR001

**Description:** Process only valid root segments with numeric PA-ACCT-ID before handling children

**Logic:** Check PA-ACCT-ID numeric after GN, skip if not

**Conditions:**
- `PA-ACCT-ID IS NUMERIC`

### BR002

**Description:** End root processing on GB status from PAUTBPCB

**Logic:** Set end flags and exit loop

**Conditions:**
- `PAUT-PCB-STATUS = 'GB'`

### BR003

**Description:** End child processing on GE status from PAUTBPCB

**Logic:** Set child end flag after no more children

**Conditions:**
- `PAUT-PCB-STATUS = 'GE'`

## Paragraphs

### MAIN-PARA

This is the primary entry point and orchestration paragraph for the entire program flow, labeled MAIN-PARA with an ENTRY 'DLITCBL' for potential dynamic invocation. It receives IMS PCBs (PAUTBPCB, PASFLPCB, PADFLPCB) via linkage section as inputs for database access. It first performs 1000-INITIALIZE to set up dates and display startup information. Then it enters a loop performing 2000-FIND-NEXT-AUTH-SUMMARY until WS-END-OF-ROOT-SEG is 'Y', handling each root segment and its children. After the loop, it performs 4000-FILE-CLOSE to log closure, though file operations are commented out. No direct business logic decisions here beyond loop control based on end flag from 2000. No explicit error handling; relies on abends from subordinates. It calls 1000-INITIALIZE for init, 2000-FIND-NEXT-AUTH-SUMMARY repeatedly for core unloading, and 4000-FILE-CLOSE for termination. Upon completion, it executes GOBACK to end the program.

**Calls:** 1000-INITIALIZE, 2000-FIND-NEXT-AUTH-SUMMARY, 4000-FILE-CLOSE

### 1000-INITIALIZE

This initialization paragraph sets up runtime variables and logs program start. It consumes system date via ACCEPT from DATE and DAY into CURRENT-DATE and CURRENT-YYDDD. It displays program start message, date, and decorative lines for audit trail. Commented code for parameter acceptance from SYSIN and sequential file opens with status checks is present but inactive. No outputs produced beyond displays and variable initialization. No business logic conditions or validations performed. No error handling implemented here. No calls to other paragraphs or programs. Control passes to 1000-EXIT upon completion.

### 2000-FIND-NEXT-AUTH-SUMMARY

This paragraph handles retrieval and processing of each root Pending Auth Summary segment, serving as the outer loop driver for unloading. It consumes the PAUTBPCB for IMS GN call with ROOT-UNQUAL-SSA to get next unqualified root segment into PENDING-AUTH-SUMMARY. If status is spaces (success), it increments read counters WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves data to OPFIL1-REC (commented write), sets ROOT-SEG-KEY from PA-ACCT-ID if numeric, performs 3100-INSERT-PARENT-SEG-GSAM to write to GSAM, resets child end flag, and loops 3000-FIND-NEXT-AUTH-DTL until no more children. On 'GB' status, sets end of root flag to exit main loop. On other errors, displays status/feedback and abends via 9999-ABEND. Business logic includes numeric check on PA-ACCT-ID to qualify processing and status-based decisions for continue/end/abend. Error handling covers non-success statuses with abend.

**Calls:** 3100-INSERT-PARENT-SEG-GSAM, 3000-FIND-NEXT-AUTH-DTL, 9999-ABEND

### 3000-FIND-NEXT-AUTH-DTL

This paragraph retrieves and processes child Pending Auth Detail segments under the current root, invoked in loop from 2000. It consumes PAUTBPCB for IMS GNP call with CHILD-UNQUAL-SSA to get next parent-qualified child into PENDING-AUTH-DETAILS. On spaces status (success), sets MORE-AUTHS flag (unused), increments counters (note: uses sumry counters erroneously?), moves to CHILD-SEG-REC, and performs 3200-INSERT-CHILD-SEG-GSAM. On 'GE' status, sets WS-END-OF-CHILD-SEG to 'Y' to exit child loop and displays flag. On other statuses, displays error/feedback and abends. Logic decisions based on PCB status for process/end/abend. Error handling abends on failures. Initializes status after call. No direct outputs beyond inserts via call.

**Calls:** 3200-INSERT-CHILD-SEG-GSAM, 9999-ABEND

### 3100-INSERT-PARENT-SEG-GSAM

This utility paragraph inserts the current root summary segment into the target GSAM database. It consumes PENDING-AUTH-SUMMARY and PASFLPCB for IMS ISRT call. On non-spaces status, displays error/KEYFB and abends via 9999-ABEND. No other inputs, outputs, or logic; purely I/O with error check. Business logic is strict success requirement for continuation. Error handling immediate abend on failure.

**Calls:** 9999-ABEND

### 3200-INSERT-CHILD-SEG-GSAM

This utility paragraph inserts the current child detail segment into the target GSAM database. It consumes PENDING-AUTH-DETAILS and PADFLPCB for IMS ISRT call. On non-spaces status, displays error/KEYFB and abends via 9999-ABEND. No other inputs, outputs, or logic; purely I/O with error check. Business logic enforces successful insert for each child. Error handling immediate abend on failure.

**Calls:** 9999-ABEND

### 4000-FILE-CLOSE

This termination paragraph logs file closure, though actual CLOSE statements for OPFILE1/2 are commented out. It consumes no data, displays 'CLOSING THE FILE'. Commented status checks and error displays. No business logic or conditions. No error handling active. No calls. Prepares for program end.

### 9999-ABEND

This error termination paragraph handles all program abends. It consumes no specific inputs, displays 'DBUNLDGS ABENDING ...' message. Sets RETURN-CODE to 16 and executes GOBACK to terminate abnormally. No validations or conditions. Called from multiple error points throughout the program. No further calls.

### 9999-EXIT

This is a minimal exit label within the abend routine, containing only an EXIT statement. It consumes no data and produces no outputs. No logic, conditions, error handling, or calls. Serves as a potential THRU exit point though not used. Provides structured exit from 9999-ABEND if needed.

## Data Flow

### Reads From

- **PAUTBPCB (PAUTSUM0 root):** PA-ACCT-ID
- **PAUTBPCB (PAUTDTL1 child):** PENDING-AUTH-DETAILS

### Writes To

- **PASFLPCB GSAM:** PENDING-AUTH-SUMMARY
- **PADFLPCB GSAM:** PENDING-AUTH-DETAILS

### Transforms

- `PA-ACCT-ID` -> `ROOT-SEG-KEY`: Move account ID from summary segment to GSAM root key field
- `PENDING-AUTH-DETAILS` -> `CHILD-SEG-REC`: Direct move of detail segment to GSAM child record

## Error Handling

- **PAUT-PCB-STATUS NOT SPACES AND NOT 'GB' after GN:** Display status/KEYFB and PERFORM 9999-ABEND
- **PAUT-PCB-STATUS NOT SPACES AND NOT 'GE' after GNP:** Display status/KEYFB and PERFORM 9999-ABEND
- **PASFL-PCB-STATUS NOT SPACES after ISRT:** Display status/KEYFB and PERFORM 9999-ABEND
- **PADFL-PCB-STATUS NOT SPACES after ISRT:** Display status/KEYFB and PERFORM 9999-ABEND
- **Any abend condition:** Set RETURN-CODE 16 and GOBACK

## Flow Diagram

```mermaid
flowchart TD
    %% Title: DBUNLDGS.CBL
    1000_EXIT["1000-EXIT"]
    1000_INITIALIZE["1000-INITIALIZE"]
    2000_EXIT["2000-EXIT"]
    2000_FIND_NEXT_AUTH_SUMMARY["2000-FIND-NEXT-AUTH-SUMMARY"]
    3000_FIND_NEXT_AUTH_DTL["3000-FIND-NEXT-AUTH-DTL"]
    3100_INSERT_PARENT_SEG_GSAM["3100-INSERT-PARENT-SEG-GSAM"]
    9999_ABEND["9999-ABEND"]
    CBLTDLI__ext(["CBLTDLI"])
    3000_EXIT["3000-EXIT"]
    3200_INSERT_CHILD_SEG_GSAM["3200-INSERT-CHILD-SEG-GSAM"]
    3100_EXIT["3100-EXIT"]
    3200_EXIT["3200-EXIT"]
    4000_EXIT["4000-EXIT"]
    4000_FILE_CLOSE["4000-FILE-CLOSE"]
    9999_EXIT["9999-EXIT"]
    MAIN_PARA["MAIN-PARA"]
    2000_FIND_NEXT_AUTH_SUMMARY --> 3000_FIND_NEXT_AUTH_DTL
    2000_FIND_NEXT_AUTH_SUMMARY --> 3100_INSERT_PARENT_SEG_GSAM
    2000_FIND_NEXT_AUTH_SUMMARY --> 9999_ABEND
    2000_FIND_NEXT_AUTH_SUMMARY -.->|calls| CBLTDLI__ext
    3000_FIND_NEXT_AUTH_DTL --> 3200_INSERT_CHILD_SEG_GSAM
    3000_FIND_NEXT_AUTH_DTL --> 9999_ABEND
    3000_FIND_NEXT_AUTH_DTL -.->|calls| CBLTDLI__ext
    3100_INSERT_PARENT_SEG_GSAM --> 9999_ABEND
    3100_INSERT_PARENT_SEG_GSAM -.->|calls| CBLTDLI__ext
    3200_INSERT_CHILD_SEG_GSAM --> 9999_ABEND
    3200_INSERT_CHILD_SEG_GSAM -.->|calls| CBLTDLI__ext
    MAIN_PARA --> 1000_INITIALIZE
    MAIN_PARA --> 2000_FIND_NEXT_AUTH_SUMMARY
    MAIN_PARA --> 4000_FILE_CLOSE
```