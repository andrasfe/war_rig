# DBUNLDGS

**File**: `cbl/DBUNLDGS.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 17:37:56.992182

## Purpose

This batch IMS DL/I program unloads root segments (PAUTSUM0 Pending Authorization Summary) sequentially from the PAUT IMS database and inserts them into the PASFL GSAM database, then unloads all child segments (PAUTDTL1 Pending Authorization Details) under each root using parentage and inserts them into the PADFL GSAM database. It processes until end of database (GB status), tracks read counts, and abends on IMS call errors. Commented sequential file outputs suggest this replaced file writes with GSAM inserts.

**Business Context**: Unloading pending authorization summary and detail data from hierarchical IMS database to flat GSAM datasets, likely for migration, backup, or processing.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (Pending Authorization Summary) read sequentially via unqualified GN calls |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment (Pending Authorization Details) read via unqualified GNP calls under current root |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFL | IOType.IMS_SEGMENT | GSAM dataset receiving inserted root summary segments |
| PADFL | IOType.IMS_SEGMENT | GSAM dataset receiving inserted child detail segments |

## Business Rules

- **BR001**: Skip processing root and its children if PA-ACCT-ID is not numeric
- **BR002**: Process children only under current root using GNP parentage

## Paragraphs/Procedures

### MAIN-PARA
This is the primary orchestration paragraph serving as the program entry point, supporting alternate entry DLITCBL for potential linkage compatibility. It receives IMS PCBs (PAUTBPCB for source DB, PASFLPCB/PADFLPCB for GSAM outputs) via linkage section. It begins by performing 1000-INITIALIZE to accept system dates, display startup messages including program name and date. Next, it enters a processing loop invoking 2000-FIND-NEXT-AUTH-SUMMARY repeatedly until WS-END-OF-ROOT-SEG is set to 'Y' (end of database reached via GB status). Within the loop, each root summary is read, validated (numeric acct ID), inserted to PASFL GSAM, and its children processed similarly to PADFL GSAM. Counters like WS-AUTH-SMRY-PROC-CNT and WS-NO-SUMRY-READ are incremented during reads. No direct data inputs beyond PCBs; outputs are GSAM inserts handled in subordinates. Error handling is delegated to called paragraphs which abend on IMS failures. After loop completion, it performs 4000-FILE-CLOSE for cleanup display (no actual files). Finally, it GOBACKs with normal return code unless abended earlier.

### 1000-INITIALIZE
This initialization paragraph sets up runtime variables and logs program start for audit trail. It accepts CURRENT-DATE from DATE and CURRENT-YYDDD from DAY into working storage fields. It displays startup banner, program name 'DBUNLDGS', separator line, and current date for operator visibility. Commented code for parameter acceptance from SYSIN and sequential file opens (OPFILE1/OPFILE2) indicates legacy or optional file-based output now replaced by GSAM inserts. No inputs consumed beyond system date; outputs are display messages to console. No business decisions or validations performed here. No error handling or abends. No subordinate calls. Control returns to caller via 1000-EXIT.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph handles sequential reading of root summary segments (PAUTSUM0) from PAUT IMS database using GN call, driving the main processing loop. It initializes PAUT-PCB-STATUS, issues CBLTDLI GN using PAUTBPCB, PENDING-AUTH-SUMMARY IO area, and unqualified ROOT-UNQUAL-SSA ('PAUTSUM0 '). If status is spaces (success), increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT counters, moves full segment to unused OPFIL1-REC, initializes ROOT-SEG-KEY/CHILD-SEG-REC, sets ROOT-SEG-KEY from PA-ACCT-ID. Then checks if PA-ACCT-ID NUMERIC; if yes, performs 3100-INSERT-PARENT-SEG-GSAM to insert summary to PASFL GSAM, resets WS-END-OF-CHILD-SEG, and loops PERFORM 3000-FIND-NEXT-AUTH-DTL until child end ('Y'). If GN returns GB (end of DB), sets END-OF-AUTHDB and WS-END-OF-ROOT-SEG to 'Y' to exit main loop. If other status, displays error/status/KEYFB and performs 9999-ABEND. Inputs from prior PCB position; outputs root insert and child loop trigger. Business logic enforces numeric acct ID for processing.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph reads child detail segments (PAUTDTL1) under the current root using GNP for parentage-qualified sequential access. It issues CBLTDLI GNP using PAUTBPCB, PENDING-AUTH-DETAILS IO area, and unqualified CHILD-UNQUAL-SSA ('PAUTDTL1 '). If status spaces, sets MORE-AUTHS flag, increments WS-NO-SUMRY-READ (likely bug, intended for details), WS-AUTH-SMRY-PROC-CNT, moves full segment to CHILD-SEG-REC (part of unused OPFIL2-REC), then performs 3200-INSERT-CHILD-SEG-GSAM to insert to PADFL GSAM. If GE (segment not found, end of children), sets WS-END-OF-CHILD-SEG to 'Y' and displays flag. If other status, displays error/status/KEYFB and abends via 9999-ABEND. Initializes PAUT-PCB-STATUS post-call. Inputs from current PCB position (after root GN); outputs child insert to GSAM. No additional validations; relies on GNP for correct parentage. Error handling aborts on failures.

### 3100-INSERT-PARENT-SEG-GSAM
This utility paragraph inserts the current root summary segment into the PASFL GSAM database. It issues CBLTDLI ISRT using PASFLPCB and PENDING-AUTH-SUMMARY IO area (full segment from prior GN). If PASFL-PCB-STATUS not spaces post-call, displays error/status/KEYFB and performs 9999-ABEND. No data transformations; direct segment insert preserving structure. Inputs full PENDING-AUTH-SUMMARY from working storage (populated by 2000 para). Outputs inserted segment to GSAM PCB. No business logic/decisions beyond status check. Strict error handling abends on any insert failure. No subordinate calls.

### 3200-INSERT-CHILD-SEG-GSAM
This utility paragraph inserts the current child detail segment into the PADFL GSAM database. It issues CBLTDLI ISRT using PADFLPCB and PENDING-AUTH-DETAILS IO area (full segment from prior GNP). If PADFL-PCB-STATUS not spaces post-call, displays error/status/KEYFB and performs 9999-ABEND. No data transformations; direct segment insert. Inputs full PENDING-AUTH-DETAILS from working storage (populated by 3000 para). Outputs inserted segment to GSAM PCB. No decisions or validations. Error handling abends immediately on failure. No calls.

### 4000-FILE-CLOSE
This termination paragraph logs file closure, though no actual files are open (commented FD/SELECT/OPEN/CLOSE). It displays 'CLOSING THE FILE' message. Commented checks for WS-OUTFL*-STATUS on close would continue or display errors if non-zero/'00'. Inputs none; outputs console display. No business logic. No error handling active. No calls. Serves as cleanup placeholder.

### 9999-ABEND
This error termination paragraph handles all program abends from IMS call failures. It displays 'DBUNLDGS ABENDING ...' message. Sets RETURN-CODE to 16 and GOBACKs, causing abnormal end. Inputs error context from caller (PCB status/KEYFB displayed prior). Outputs abend message and non-zero return code. No recovery or logging beyond display. Called by all error paths in read/insert paras.

## Open Questions

- ? Exact layout and fields of segments defined by CIPAUSMY and CIPAUDTY copybooks
  - Context: Copybooks referenced but source not provided; only names and usage inferred
- ? Purpose of unused OPFIL1-REC and OPFIL2-REC moves and commented file I/O
  - Context: Records defined and moved to (236,280) but no OPEN/WRITE/CLOSE executed
- ? Why WS-NO-SUMRY-READ incremented in child paragraph (278)
  - Context: Increments summary read counter during detail processing; likely copy-paste error vs WS-NO-DTL-READ
- ? Usage of expiry/date/chkpt variables (WS-EXPIRY-DAYS, WS-AUTH-DATE, etc.)
  - Context: Defined and partially initialized but never used in logic
