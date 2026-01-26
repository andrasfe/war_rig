# DBUNLDGS

**File**: `cbl/DBUNLDGS.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 02:30:25.680999

## Purpose

This IMS batch program sequentially reads all root segments (PAUTSUM0) from the PAUT IMS database using GN calls and inserts them into the PASFL GSAM database using ISRT. For each root, it then reads dependent child segments (PAUTDTL1) using GNP calls and inserts them into the PADFL GSAM database. It continues until end of database (GB status) and handles errors by abending.

**Business Context**: Unloading pending authorization summary and detail records from IMS PAUT database to GSAM for potential archiving, migration, or fast-path access in a financial authorization system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.IMS_SEGMENT | IMS database PCB for PAUT database providing root PAUTSUM0 and child PAUTDTL1 segments via unqualified SSA sequential reads |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFLPCB | IOType.IMS_SEGMENT | GSAM database PCB receiving inserted PAUTSUM0 root segments from PAUT |
| PADFLPCB | IOType.IMS_SEGMENT | GSAM database PCB receiving inserted PAUTDTL1 child segments from PAUT |

## Business Rules

- **BR001**: Abort program if root GN call returns status other than spaces (success) or GB (end of DB)
- **BR002**: Abort program if child GNP call returns status other than spaces (success) or GE (segment not found/end children)
- **BR003**: Abort program if ISRT to PASFL GSAM fails (status not spaces)
- **BR004**: Abort program if ISRT to PADFL GSAM fails (status not spaces)

## Paragraphs/Procedures

### MAIN-PARA
This is the primary orchestration paragraph serving as the program entry point via ENTRY 'DLITCBL' using the three linkage PCBs. It consumes no direct inputs but relies on the provided IMS PCBs for database access. It first invokes 1000-INITIALIZE to accept system dates and issue startup display messages. It then enters a main processing loop that repeatedly performs 2000-FIND-NEXT-AUTH-SUMMARY until WS-END-OF-ROOT-SEG is set to 'Y' indicating end of root segments. Within the loop, root processing includes child detail reads via 3000-FIND-NEXT-AUTH-DTL. After loop completion, it performs 4000-FILE-CLOSE for cleanup displays (files commented out). No direct business logic decisions here; flow control is sequential with loop exit on EOF flag. Error handling is delegated to subordinate paragraphs which abend on failures. It produces no direct outputs but coordinates all IMS inserts indirectly. Counters like WS-NO-SUMRY-READ are incremented in called paragraphs.

### 1000-INITIALIZE
This initialization paragraph sets up runtime variables and issues informational displays. It consumes system date via ACCEPT from DATE and DAY into CURRENT-DATE and CURRENT-YYDDD. No files are opened as code is commented out. It displays program start message, separator, and current date. No business logic or decisions are implemented here. No error handling as no I/O attempted. No paragraphs or programs called. It produces display output to console/sysout and initializes WS variables implicitly via prior SECTION. Role is preparatory housekeeping before main processing loop.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph handles sequential read of next root PAUTSUM0 segment from PAUT IMS database using unqualified SSA GN call via CBLTDLI. It consumes PAUTBPCB linkage and prior SSA definitions, reading into PENDING-AUTH-SUMMARY. On success (spaces status), increments counters WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves data to WS OPFIL1-REC (write commented), sets ROOT-SEG-KEY from PA-ACCT-ID if numeric, performs 3100-INSERT-PARENT-SEG-GSAM to write to GSAM, resets child EOF, then loops 3000-FIND-NEXT-AUTH-DTL until child end. On GB status, sets end flags for root loop exit. On other statuses, displays error/KEYFB and abends via 9999-ABEND. Business logic validates numeric PA-ACCT-ID before processing. Error handling aborts on unexpected IMS statuses. Calls subordinate insert and child read paragraphs to complete root processing. Produces inserted root segment in PASFL GSAM.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph performs sequential parent-qualified read of next child PAUTDTL1 segment using GNP call via CBLTDLI with CHILD-UNQUAL-SSA. It consumes PAUTBPCB and reads into PENDING-AUTH-DETAILS. On success (spaces), sets MORE-AUTHS flag (unused), increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT (likely misnamed for details), moves to CHILD-SEG-REC, performs 3200-INSERT-CHILD-SEG-GSAM. On GE status, sets WS-END-OF-CHILD-SEG to 'Y' to exit child loop and displays flag. On other statuses, displays error/KEYFB and abends. No additional validations or transforms. Error handling aborts on invalid statuses. Called repeatedly from 2000 for each root's children. Produces inserted child segments in PADFL GSAM and controls child loop exit.

### 3100-INSERT-PARENT-SEG-GSAM
This utility paragraph inserts the current PENDING-AUTH-SUMMARY root segment into PASFL GSAM database using ISRT call via CBLTDLI on PASFLPCB. It consumes PENDING-AUTH-SUMMARY populated from prior GN read. No data transforms, direct insert. Checks PASFL-PCB-STATUS post-call; if not spaces, displays error/KEYFB and abends via 9999-ABEND. No business decisions or validations beyond IMS status. Error handling is strict abend on failure. Called from 2000 after each root read. Produces new segment in PASFL GSAM. Minimal logic focused on I/O reliability.

### 3200-INSERT-CHILD-SEG-GSAM
This utility paragraph inserts the current PENDING-AUTH-DETAILS child segment into PADFL GSAM database using ISRT call via CBLTDLI on PADFLPCB. It consumes PENDING-AUTH-DETAILS from prior GNP read. Direct insert with no transforms. Post-call checks PADFL-PCB-STATUS; if not spaces, displays error/KEYFB and abends. No conditions or business rules beyond status check. Strict error handling via abend. Called from 3000 for each child. Ensures reliable write of detail records dependent on parent.

### 4000-FILE-CLOSE
This termination paragraph handles program cleanup by displaying 'CLOSING THE FILE' message. No actual file closes as OPEN/CLOSE code commented out. Consumes no inputs. No I/O status checks active. No errors possible in current code. No calls made. Role is end-of-job notification, though vestigial due to comments. Produces console display.

### 9999-ABEND
This error termination paragraph is invoked on all IMS call failures. It displays 'DBUNLDGS ABENDING ...' message. Sets RETURN-CODE to 16. Performs GOBACK to exit program. Consumes no specific data. No recovery attempts. Called from error branches in 2000, 3000, 3100, 3200. Ensures abnormal end with user message and non-zero RC.

## Open Questions

- ? Exact field layouts and keys in copybooks CIPAUSMY, CIPAUDTY, PAUTBPCB, PASFLPCB, PADFLPCB
  - Context: Copybooks referenced but contents not expanded in source; limits field-level data flow details
- ? Purpose of unused flags/counters like WS-NO-CHKP, WS-MORE-AUTHS-FLAG, WS-NO-DTL-READ
  - Context: Defined and partially incremented but not used in visible logic
- ? Why counters WS-NO-SUMRY-READ/WS-AUTH-SMRY-PROC-CNT incremented in child paragraph 3000
  - Context: Appears misnamed or erroneous as it increments on details not summaries (line 278)
