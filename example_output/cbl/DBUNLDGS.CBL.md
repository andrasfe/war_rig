# DBUNLDGS

**File**: `cbl/DBUNLDGS.CBL`
**Type**: COBOL
**Analyzed**: 2026-01-26 14:23:49.950346

## Purpose

Unloads root segments (PAUTSUM0 Pending Authorization Summary) from IMS database PAUT via unqualified GN calls, incrementing read counters for all roots found. Only inserts valid roots into GSAM PASFL and processes child details (PAUTDTL1) into GSAM PADFL if PA-ACCT-ID is numeric; skips insert and children otherwise but still counts the root read. Continues until end of database (GB status), abending on other IMS errors.

**Business Context**: Facilitates unloading of pending authorization data from hierarchical IMS database PAUT to flat GSAM files PASFL/PADFL for backup, migration, or processing in an authorization system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUT | IMS_SEGMENT | IMS database PCB PAUTBPCB providing root PAUTSUM0 summary segments via GN and child PAUTDTL1 detail segments via GNP |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFL | IMS_SEGMENT | GSAM PCB PASFLPCB receiving inserted root summary segments from PAUTSUM0 |
| PADFL | IMS_SEGMENT | GSAM PCB PADFLPCB receiving inserted child detail segments from PAUTDTL1 |

## Business Rules

- **BR001**: Skip insertion of root summary segment into PASFL and processing of its child details if PA-ACCT-ID is not numeric; still increment read counters for the root
- **BR002**: Increment summary read and process counters for every successful root GN regardless of numeric check

## Paragraphs/Procedures

### MAIN-PARA
MAIN-PARA serves as the primary entry and orchestration paragraph for the entire unload process, invoked via ENTRY 'DLITCBL' using linkage PCBs PAUTBPCB, PASFLPCB, PADFLPCB. It consumes no direct data inputs but relies on the passed IMS PCBs for database access. It first calls 1000-INITIALIZE to accept and display current date information for logging. It then enters a main processing loop calling 2000-FIND-NEXT-AUTH-SUMMARY repeatedly until WS-END-OF-ROOT-SEG is 'Y', processing all root summaries and conditionally their children. After the loop completes upon end-of-database, it performs 4000-FILE-CLOSE for cleanup display, though actual file closes are commented out. There is no local business logic or decisions; flow is strictly sequential orchestration. No error handling here; subordinate paragraphs handle IMS errors by abending to 9999-ABEND. It produces updated WS counters like WS-NO-SUMRY-READ accumulated from children paragraphs. It calls 1000-INITIALIZE for setup, 2000-FIND-NEXT-AUTH-SUMMARY in loop for core reading/processing, and 4000-FILE-CLOSE for termination.

### 1000-INITIALIZE
This initialization paragraph sets up runtime variables and logs startup information. It consumes system date via ACCEPT CURRENT-DATE FROM DATE and CURRENT-YYDDD FROM DAY. It displays program start message, today's date, and decorative lines for operator console logging. No files are opened as those statements are commented out. No outputs are produced beyond displays. No business logic or conditions are evaluated. No error handling is performed. It calls no other paragraphs. Control returns immediately to caller after displays.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph performs the core root segment retrieval and conditional processing loop trigger for each PAUTSUM0 summary. It consumes root segments from PAUTBPCB via unqualified GN call using ROOT-UNQUAL-SSA 'PAUTSUM0 '. On success (PAUT-PCB-STATUS SPACES), it increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT counters unconditionally, moves full PENDING-AUTH-SUMMARY to OPFIL1-REC buffer, and sets ROOT-SEG-KEY from PA-ACCT-ID. It then checks IF PA-ACCT-ID IS NUMERIC; if true, performs 3100-INSERT-PARENT-SEG-GSAM to insert into PASFLPCB and initializes/resets child flags before looping PERFORM 3000-FIND-NEXT-AUTH-DTL until WS-END-OF-CHILD-SEG='Y' to process all siblings; if false, skips insert and children entirely. On GB status, sets end flags to exit main loop. On other statuses, displays error details (status, KEYFB) and abends via 9999-ABEND. Business logic enforces data validity via numeric check on account ID before unload. Error handling covers all non-success/GB IMS responses. It calls 3100-INSERT-PARENT-SEG-GSAM conditionally for output and 3000-FIND-NEXT-AUTH-DTL in loop for children.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph retrieves and unloads child detail segments PAUTDTL1 under current root via unqualified GNP from PAUTBPCB using CHILD-UNQUAL-SSA 'PAUTDTL1 '. It consumes sequential children post-parent GN positioning. On success (SPACES), sets MORE-AUTHS flag, increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT (note: summary counters despite details), moves PENDING-AUTH-DETAILS to CHILD-SEG-REC buffer, and performs 3200-INSERT-CHILD-SEG-GSAM to output to PADFLPCB. On GE status, sets WS-END-OF-CHILD-SEG='Y' to exit child loop and displays flag. On other statuses, displays error (status, KEYFB) and abends via 9999-ABEND. No additional business logic beyond status handling. Error handling rejects non-SPACES/GE responses. Called repeatedly in loop from 2000 until child end; initializes PAUT-PCB-STATUS at end for next call. It calls 3200-INSERT-CHILD-SEG-GSAM for each valid child insert.

### 3100-INSERT-PARENT-SEG-GSAM
This utility paragraph inserts a single root summary segment into output GSAM PASFLPCB. It consumes the current PENDING-AUTH-SUMMARY (with ROOT-SEG-KEY set) via ISRT call using FUNC-ISRT. It produces the inserted segment in PASFL if successful. No transformations or conditions; assumes data validity from caller. On non-SPACES PASFL-PCB-STATUS, displays error (status, KEYFB) and abends via 9999-ABEND. No local error recovery. Called only from 2000 if PA-ACCT-ID numeric.

### 3200-INSERT-CHILD-SEG-GSAM
This utility paragraph inserts a single child detail segment into output GSAM PADFLPCB. It consumes the current PENDING-AUTH-DETAILS (with parent key feedback assumed) via ISRT call using FUNC-ISRT. It produces the inserted segment in PADFL if successful. No transformations or conditions. On non-SPACES PADFL-PCB-STATUS, displays error (status, KEYFB) and abends via 9999-ABEND. No local error recovery. Called from 3000 for each valid child.

### 4000-FILE-CLOSE
This termination paragraph handles program cleanup. It consumes no data. It displays 'CLOSING THE FILE' message; actual CLOSE statements for commented files are absent. No outputs produced. No conditions or logic. No error handling. Called once from MAIN-PARA post-processing.

### 9999-ABEND
This error termination paragraph abends the program on IMS or other failures. It consumes no inputs. It displays 'DBUNLDGS ABENDING ...', sets RETURN-CODE to 16, and GOBACKs. No recovery logic. Called from error paths in 2000, 3000, 3100, 3200.

## Open Questions

- ? Exact field layout and other fields in PA-ACCT-ID, PENDING-AUTH-SUMMARY, PENDING-AUTH-DETAILS
  - Context: Defined in unprovided copybooks CIPAUSMY/CIPAUDTY
- ? Purpose of counters WS-NO-DTL-READ/DELETED and WS-TOT-REC-WRITTEN (defined but never incremented)
  - Context: Defined lines 71-73 but no ADD statements found
