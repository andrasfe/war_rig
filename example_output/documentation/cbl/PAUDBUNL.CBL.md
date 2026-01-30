# PAUDBUNL

**File**: `cbl/PAUDBUNL.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-30 19:29:49.021786

## Purpose

PAUDBUNL unloads authorization summary records from an IMS database by performing DL/I GN calls on root segments, writing each summary to OPFILE1 after extracting account ID, and then processing child detail segments via a subordinate paragraph until no more children. It initializes by opening output files OPFILE1 and OPFILE2 with status checks, loops until end of root segments (IMS status 'GB'), and closes files at termination. The program displays startup information including current date.

**Business Context**: Unloading IMS authorization database (summary and detail records) to sequential output files for reporting or archiving

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.IMS_SEGMENT | IMS PCB used for DL/I calls to read authorization summary root segments and child details from AUTHDB database |
| ROOT-UNQUAL-SSA | IOType.IMS_SEGMENT | Unqualified SSA for IMS root segment access |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OPFILE1 | IOType.FILE_SEQUENTIAL | Sequential output file containing authorization summary records (moved from PENDING-AUTH-SUMMARY) and potentially child details |
| OPFILE2 | IOType.FILE_SEQUENTIAL | Sequential output file opened but not written in provided code snippet; purpose unclear |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| CBLTDLI | CallType.DYNAMIC_CALL | Executes IMS DL/I function GN (Get Next) to retrieve next root authorization summary segment |
| 9999-ABEND | CallType.STATIC_CALL | Terminates program abnormally on file open or IMS status errors |

## Business Rules

- **BR001**: Validate OPFILE1 open status before proceeding
- **BR002**: Validate OPFILE2 open status before proceeding
- **BR003**: Process IMS root segment only on successful read (status spaces)
- **BR004**: Detect end of IMS root segments
- **BR005**: Abort on IMS read error (neither spaces nor 'GB')

## Paragraphs/Procedures

### PAUDBUNL
UNKNOWN - No code visible at specified lines 18-18; potentially the program entry point or label preceding 1000-INITIALIZE. Cannot determine inputs, outputs, logic, or calls from provided snippet. May orchestrate overall flow or serve as IMS entry convention. No error handling visible. No subordinate calls identifiable.

### MAIN-PARA
MAIN-PARA is the primary orchestration paragraph controlling the program's overall execution flow starting after the ENTRY 'DLITCBL' USING PAUTBPCB. It consumes the IMS PCB from linkage for subordinate processing. It first invokes 1000-INITIALIZE THRU 1000-EXIT to handle startup including date acceptance, displays, and output file opens with status validation/abend on failure. It then enters a loop invoking 2000-FIND-NEXT-AUTH-SUMMARY THRU 2000-EXIT until WS-END-OF-ROOT-SEG is 'Y', processing all root authorization summary segments and their children. Upon loop exit, it performs 4000-FILE-CLOSE THRU 4000-EXIT for cleanup. The business logic enforces sequential initialization, iterative unloading until database EOF, and termination. Error handling is delegated to called paragraphs like 1000 and 2000 which abend on issues. It produces no direct outputs but coordinates writes to OPFILE1/2 via subordinates. Finally, it executes GOBACK to return control.

### 1000-INITIALIZE
1000-INITIALIZE handles program startup and output file setup. It consumes system date via ACCEPT CURRENT-DATE FROM DATE and CURRENT-YYDDD FROM DAY. It displays startup banner, program name, and date for audit trail. It opens OPFILE1 OUTPUT and checks WS-OUTFL1-STATUS; if not spaces or '00', displays error and performs 9999-ABEND. Similarly opens OPFILE2 OUTPUT and repeats status check/abend logic. Business logic ensures files are ready before processing, preventing downstream I/O failures. No loops or complex transforms. Error handling is explicit file status checks with immediate abend on failure. It produces initialized files and display output. No calls to other programs; delegates abend only.

### 1000-EXIT
1000-EXIT is a simple exit paragraph that returns control to the caller (MAIN-PARA) after initialization completes successfully. It consumes no data. It produces no outputs or modifications. No business logic, decisions, or error handling. No calls made.

### 2000-FIND-NEXT-AUTH-SUMMARY
2000-FIND-NEXT-AUTH-SUMMARY retrieves and processes the next IMS authorization summary root segment, serving as the core loop body for unloading. It consumes IMS PCB (PAUTBPCB) and SSA via CALL 'CBLTDLI' USING FUNC-GN, populating PENDING-AUTH-SUMMARY and status fields. On success (PAUT-PCB-STATUS = SPACES), it increments read/process counters WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves summary to OPFIL1-REC, initializes ROOT-SEG-KEY and CHILD-SEG-REC, sets ROOT-SEG-KEY from PA-ACCT-ID, and if PA-ACCT-ID numeric, writes OPFIL1-REC then initializes/performs 3000-FIND-NEXT-AUTH-DTL THRU 3000-EXIT loop until WS-END-OF-CHILD-SEG='Y' to handle children. On 'GB' status, sets end flags WS-END-OF-ROOT-SEG='Y' and END-OF-AUTHDB TRUE to exit main loop. On other statuses, displays error details (status, PAUT-KEYFB) and abends via 9999-ABEND. Business logic implements IMS navigation (GN for sequential root access), conditional child processing only on valid numeric account, and comprehensive status-based branching for success/EOF/error. Error handling covers IMS PCB status and file status implicitly via children.

### 2000-EXIT
This paragraph serves as a simple exit point, likely concluding a prior processing routine such as a main loop or initialization section before transitioning to child segment retrieval. It consumes no inputs or data explicitly, acting as a control flow transfer mechanism with no reads from files, variables, or IMS. It produces no outputs, modifications, or writes, simply returning control to the calling paragraph. No business logic, decisions, or conditions are evaluated here. No error handling is performed as it is a no-op. It calls no other paragraphs or programs. The paragraph ensures clean flow termination from the 2000 section without side effects. In the broader program context, it likely follows a perform of 2000-something-main, handing off to 3000-FIND-NEXT-AUTH-DTL for child processing.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph's primary purpose is to retrieve the next child authorization detail segment from the IMS database using a GNP DL/I call, process it if valid, and handle completion or error statuses to control loop continuation. It consumes positioning from the PAUTBPCB (parent PCB), the PENDING-AUTH-DETAILS IO area for data receipt, and CHILD-UNQUAL-SSA for the call; it reads PAUT-PCB-STATUS post-call to determine success. It produces output by writing valid segments (moved to CHILD-SEG-REC) to OPFILE2, increments counters WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, sets WS-END-OF-CHILD-SEG flag on 'GE' status, and initializes PAUT-PCB-STATUS for next call. Business logic implements IMS status checks: spaces means valid segment (set MORE-AUTHS true and write), 'GE' ends child processing, other statuses error with display and abend. Error handling displays failure details (status, KEYFB) and performs 9999-ABEND on invalid status. It calls CBLTDLI for IMS access and 9999-ABEND on error. This enables iterative child segment unloading under a parent.

### 3000-EXIT
This paragraph acts as the exit point for the 3000-FIND-NEXT-AUTH-DTL routine, facilitating return of control to the invoking perform statement, typically a loop checking WS-END-OF-CHILD-SEG. It consumes no data, performing no reads from IMS, files, or variables. It modifies no working storage, files, or outputs. No conditional logic or business decisions are made. Error handling is absent as it is purely a flow control exit. No subordinate calls to paragraphs or programs occur. In program flow, it ensures the GNP processing completes cleanly before re-evaluation of loop conditions like MORE-AUTHS or end flags. This prevents residual processing and maintains structured control.

### 4000-FILE-CLOSE
The primary role of this paragraph is to gracefully close the output files OPFILE1 and OPFILE2 at program termination, ensuring data integrity and resource release. It consumes file status feedback into WS-OUTFL1-STATUS and WS-OUTFL2-STATUS post-close. It produces display messages on errors but no data writes; files are closed without further output. Business logic checks each file status: if spaces or '00', continue normally; else display error message with status. Error handling logs close failures via DISPLAY but does not abend, allowing program to end. No calls to other paragraphs or programs. Displays 'CLOSING THE FILE' upfront. This paragraph is invoked at EOP to finalize batch output processing.

### 4000-EXIT
This serves as the exit from the 4000-FILE-CLOSE paragraph, returning control to the main termination logic or GOBACK. No inputs are read or consumed. No outputs, writes, or modifications occur. No decisions, conditions, or business rules applied. No error handling needed. No calls made. It provides a clean endpoint for file closure activities, ensuring flow proceeds to program end.

### 9999-ABEND
This paragraph serves as the abnormal termination handler for the PAUDBUNL program, invoked when a critical error requires the program to abend. It consumes no specific inputs or data from files or variables, relying solely on its fixed logic. It produces a console display message 'IMSUNLOD ABENDING ...' for logging and diagnostics (line 4). It also sets the RETURN-CODE to 16 to communicate the abend condition to the caller or job control (line 6). Finally, it executes GOBACK to terminate program execution (line 7). No business logic or conditional decisions are implemented; it performs unconditional termination actions. No validation or further error handling occurs within this paragraph, as it is the final error routine. It does not call any other paragraphs or external programs. The presence of comments (lines 2-3) suggests it is part of a larger IMS-related unload process based on the message content.

### ~~9999-EXIT~~ (Dead Code)
*Paragraph '9999-EXIT' is never PERFORMed or referenced by any other paragraph or program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| 9999-EXIT | paragraph | 316 | Paragraph '9999-EXIT' is never PERFORMed or referenced by any other paragraph or program |

## Control Flow

```mermaid
flowchart TD
    %% Title: PAUDBUNL.CBL
    1000_EXIT["1000-EXIT"]
    1000_INITIALIZE["1000-INITIALIZE"]
    9999_ABEND["9999-ABEND"]
    2000_EXIT["2000-EXIT"]
    2000_FIND_NEXT_AUTH_SUMMARY["2000-FIND-NEXT-AUTH-SUMMARY"]
    3000_FIND_NEXT_AUTH_DTL["3000-FIND-NEXT-AUTH-DTL"]
    CBLTDLI__ext(["CBLTDLI"])
    3000_EXIT["3000-EXIT"]
    4000_EXIT["4000-EXIT"]
    4000_FILE_CLOSE["4000-FILE-CLOSE"]
    9999_EXIT["9999-EXIT"]
    MAIN_PARA["MAIN-PARA"]
    1000_INITIALIZE --> 9999_ABEND
    2000_FIND_NEXT_AUTH_SUMMARY --> 3000_FIND_NEXT_AUTH_DTL
    2000_FIND_NEXT_AUTH_SUMMARY --> 9999_ABEND
    2000_FIND_NEXT_AUTH_SUMMARY -.->|calls| CBLTDLI__ext
    3000_FIND_NEXT_AUTH_DTL --> 9999_ABEND
    3000_FIND_NEXT_AUTH_DTL -.->|calls| CBLTDLI__ext
    MAIN_PARA --> 1000_INITIALIZE
    MAIN_PARA --> 2000_FIND_NEXT_AUTH_SUMMARY
    MAIN_PARA --> 4000_FILE_CLOSE
```

## Open Questions

- ? What does 3000-FIND-NEXT-AUTH-DTL paragraph do exactly?
  - Context: PERFORM referenced at line 78 but source code not provided
- ? What is the purpose and content of OPFILE2?
  - Context: Opened at line 39 but never written in snippet
- ? Details of 4000-FILE-CLOSE and 9999-ABEND?
  - Context: Referenced but code not provided
- ? Copybook definitions for records like PENDING-AUTH-SUMMARY, OPFIL1-REC?
  - Context: No COPY statements visible

## Sequence Diagram

```mermaid
sequenceDiagram
    PAUDBUNL->>IMSFUNCS: performs
    PAUDBUNL->>CIPAUSMY: performs
    PAUDBUNL->>CIPAUDTY: performs
    PAUDBUNL->>PAUTBPCB: performs
    MAIN-PARA->>1000-INITIALIZE: performs
    1000-INITIALIZE-->>MAIN-PARA: CURRENT-DATE, CURRENT-YYDDD, WS-OUTFL1-STATUS, ...
    MAIN-PARA->>2000-FIND-NEXT-AUTH-SUMMARY: performs
    2000-FIND-NEXT-AUTH-SUMMARY-->>MAIN-PARA: WS-NO-SUMRY-READ, WS-AUTH-SMRY-PROC-CNT, WS-END-OF-CHILD-SEG, ...
    MAIN-PARA->>4000-FILE-CLOSE: performs
    4000-FILE-CLOSE-->>MAIN-PARA: WS-OUTFL1-STATUS, WS-OUTFL2-STATUS
    1000-INITIALIZE->>9999-ABEND: performs
    9999-ABEND-->>1000-INITIALIZE: RETURN-CODE
    1000-INITIALIZE->>9999-ABEND: performs
    9999-ABEND-->>1000-INITIALIZE: RETURN-CODE
    2000-FIND-NEXT-AUTH-SUMMARY->>CBLTDLI: performs
    2000-FIND-NEXT-AUTH-SUMMARY->>3000-FIND-NEXT-AUTH-DTL: WS-NO-SUMRY-READ, WS-AUTH-SMRY-PROC-CNT
    3000-FIND-NEXT-AUTH-DTL-->>2000-FIND-NEXT-AUTH-SUMMARY: WS-NO-SUMRY-READ, WS-AUTH-SMRY-PROC-CNT, WS-MORE-AUTHS-FLAG, ...
    2000-FIND-NEXT-AUTH-SUMMARY->>9999-ABEND: performs
    3000-FIND-NEXT-AUTH-DTL->>CBLTDLI: performs
    3000-FIND-NEXT-AUTH-DTL->>9999-ABEND: performs
```
