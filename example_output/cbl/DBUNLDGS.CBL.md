# DBUNLDGS

**File**: `cbl/DBUNLDGS.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 15:14:33.487315

## Purpose

This IMS DL/I program unloads pending authorization summary root segments (PAUTSUM0) and their dependent detail child segments (PAUTDTL1) from the PAUT IMS database. For each root segment retrieved successfully where PA-ACCT-ID is numeric, it inserts the summary segment into the PASFL GSAM database and then retrieves and inserts all associated child detail segments into the PADFL GSAM database. Root segments with non-numeric PA-ACCT-ID are read and counted but skipped for insertion and child processing.

**Business Context**: Supports unloading of pending authorization data from hierarchical IMS database to flat GSAM structures, likely for backup, migration, or batch processing of authorization summaries and details.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.IMS_SEGMENT | IMS PCB for PAUT database providing access to root summary segments (PAUTSUM0) and child detail segments (PAUTDTL1) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFLPCB | IOType.IMS_SEGMENT | GSAM PCB for inserting unloaded root summary segments from PAUTSUM0 |
| PADFLPCB | IOType.IMS_SEGMENT | GSAM PCB for inserting unloaded child detail segments from PAUTDTL1 with root key prepended |

## Business Rules

- **BR001**: Only unload root segment and its children if PA-ACCT-ID is numeric
- **BR002**: End root segment processing on IMS status 'GB' (end of database)
- **BR003**: End child segment processing on IMS status 'GE' (segment not found)

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point paragraph that orchestrates the entire unload process for the IMS database. It first establishes an alternate entry point 'DLITCBL' for potential dynamic calls, passing the three PCBs in linkage. It consumes no direct inputs but relies on the passed PCBs for database access. It performs 1000-INITIALIZE to set up dates and display startup information. Then it enters a loop performing 2000-FIND-NEXT-AUTH-SUMMARY to retrieve and process each root summary segment until WS-END-OF-ROOT-SEG is 'Y'. For each valid root, subordinate paragraphs handle child details. After all roots are processed, it performs 4000-FILE-CLOSE for cleanup displays. No direct outputs are produced here, but controls flow to insertions via called paragraphs. Error handling is delegated to subordinates which abend on failures. It calls 1000-INITIALIZE for init, 2000-FIND-NEXT-AUTH-SUMMARY in loop for core processing, and 4000-FILE-CLOSE for termination.

### 1000-INITIALIZE
This initialization paragraph sets up runtime variables and logs startup information. It consumes system date via ACCEPT from DATE and DAY into CURRENT-DATE and CURRENT-YYDDD. No files are opened as related code is commented out. It produces display output showing program start, today's date, and decorative messages for monitoring. No business decisions are made here. No error handling beyond potential ACCEPT failures which are unhandled. It calls no other paragraphs. Control returns to caller after displays. This paragraph has no validation or conditions checked.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph retrieves the next root summary segment from the PAUT IMS database using GN call. It consumes the PAUTBPCB and ROOT-UNQUAL-SSA for unqualified GN call, initializing PCB status first. On success (spaces status), it increments read counters WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves segment to OPFIL1-REC structure. It checks if PA-ACCT-ID is numeric; if yes, moves to ROOT-SEG-KEY, performs 3100-INSERT-PARENT-SEG-GSAM to insert into PASFL GSAM, resets child end flag, and loops 3000-FIND-NEXT-AUTH-DTL until child end. If status 'GB', sets end flags for root loop exit. If other error status, displays error details and abends via 9999-ABEND. Outputs are counters updated and GSAM inserts for valid roots. Business logic enforces numeric account ID filter before processing children. Error handling abends on unexpected IMS statuses.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph retrieves the next child detail segment dependent on current root using GNP call. It consumes PAUTBPCB and CHILD-UNQUAL-SSA, targeting PAUTDTL1 under current parent. On success (spaces), increments WS-NO-DTL-READ? (wait, code shows WS-NO-SUMRY-READ again, likely typo), sets MORE-AUTHS flag, moves segment to CHILD-SEG-REC, performs 3200-INSERT-CHILD-SEG-GSAM to insert into PADFL GSAM. If 'GE', sets WS-END-OF-CHILD-SEG to 'Y' to exit child loop. If other status, displays error and abends. Outputs are child inserts and counters. No additional business decisions beyond IMS status checks. Error handling abends on failures, reinitializes PCB status at end. Called repeatedly in 2000 loop until child end.

### 3100-INSERT-PARENT-SEG-GSAM
This utility paragraph inserts the current root summary segment into the PASFL GSAM database. It consumes PENDING-AUTH-SUMMARY segment data and PASFLPCB. Performs ISRT DL/I call using FUNC-ISRT. Produces insertion into GSAM if successful (spaces status). If non-spaces status, displays error with PCB status and key feedback, then abends via 9999-ABEND. No business logic or conditions beyond status check. No validations on data. Called only for numeric PA-ACCT-ID roots from 2000 paragraph.

### 3200-INSERT-CHILD-SEG-GSAM
This utility paragraph inserts the current child detail segment into the PADFL GSAM database. It consumes PENDING-AUTH-DETAILS with prepended ROOT-SEG-KEY and PADFLPCB. Performs ISRT DL/I call using FUNC-ISRT. Produces insertion into GSAM if successful. If non-spaces status, displays error details and abends. No additional logic or validations. Called for each child under valid roots from 3000 paragraph.

### 4000-FILE-CLOSE
This termination paragraph handles cleanup, primarily displaying 'CLOSING THE FILE' message. No actual file closes as code is commented out. Consumes no data. Produces display for monitoring. No errors checked or handled here. No business logic. Called once at end of MAIN-PARA after all processing.

### 9999-ABEND
This error termination paragraph abends the program on any failure. Consumes no inputs. Displays 'DBUNLDGS ABENDING ...', sets RETURN-CODE to 16, and GOBACKs. No recovery or logging beyond display. Called from error branches in 2000, 3000, 3100, 3200 on IMS failures. Handles all abend cases uniformly.

## Open Questions

- ? Exact field layouts in copybooks CIPAUSMY and CIPAUDTY
  - Context: Copybooks not provided in source; only PA-ACCT-ID referenced explicitly
- ? Purpose of unused counters like WS-NO-CHKP, WS-NO-SUMRY-DELETED, WS-NO-DTL-READ, WS-NO-DTL-DELETED
  - Context: Defined but not incremented or used in active code
- ? Role of commented file I/O (OPFILE1, OPFILE2) vs active GSAM inserts
  - Context: Sequential file logic commented; GSAM via IMS active
