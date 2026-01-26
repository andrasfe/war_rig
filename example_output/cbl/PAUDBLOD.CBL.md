# PAUDBLOD

**File**: `cbl/PAUDBLOD.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 02:30:14.312093

## Purpose

This batch COBOL program loads pending authorization data into an IMS hierarchical database by reading root segments (PAUTSUM0) from sequential file INFILE1 and inserting them via IMS ISRT calls, then reading child segments (PAUTDTL1) from INFILE2, qualifying by root key, performing GU on the root, and inserting the children via ISRT. It handles file I/O status checks and IMS PCB feedback status, abending on errors other than successful insert or duplicate (II). The program uses unqualified SSA for root inserts and child inserts, qualified SSA for root GU.

**Business Context**: Supports loading of pending authorization summary and detail records into IMS database for authorization processing, likely part of a mainframe financial or payment system handling authorizations.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | IOType.FILE_SEQUENTIAL | Sequential file containing root segment records (PAUTSUM0 pending authorization summary) in fixed 100-byte format |
| INFILE2 | IOType.FILE_SEQUENTIAL | Sequential file containing child segment records prefixed by root segment key (S9(11) COMP-3) followed by 200-byte child data (PAUTDTL1 pending authorization details) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (pending authorization summary) inserted into IMS database |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment (pending authorization details) inserted under matching PAUTSUM0 root in IMS database |

## Business Rules

- **BR001**: Skip root segment insert if duplicate (PCB status 'II'), treat as success
- **BR002**: Only process child record if ROOT-SEG-KEY is numeric
- **BR003**: Require successful GU on root (status spaces) before child ISRT, abend on other failures
- **BR004**: Skip child segment insert if duplicate (PCB status 'II'), treat as success

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph that controls the entire program flow for IMS database loading. It begins with an ENTRY statement 'DLITCBL' USING PAUTBPCB to allow invocation as a subprogram with IMS PCB. It displays a start message, then calls 1000-INITIALIZE to set dates, open input files INFILE1 and INFILE2, and validate open status. Next, it enters a loop performing 2000-READ-ROOT-SEG-FILE until END-ROOT-SEG-FILE = 'Y', processing all root segments from INFILE1 by inserting them into IMS. After exhausting roots, it enters another loop performing 3000-READ-CHILD-SEG-FILE until END-CHILD-SEG-FILE = 'Y', processing child segments from INFILE2 by GU-ing the parent root and inserting children. Upon completion of both loops, it performs 4000-FILE-CLOSE to shut down files with status checks. No explicit error handling beyond abends in subordinates; it relies on flags set in child paragraphs. Finally, it GOBACKs with return code determined by prior abends. This sequential processing assumes INFILE1 contains all roots first and INFILE2 children keyed appropriately.

### 1000-INITIALIZE
This initialization paragraph prepares the environment by accepting system dates into CURRENT-DATE and CURRENT-YYDDD, displaying them for logging. It opens INPUT files INFILE1 and INFILE2, checking WS-INFIL1-STATUS and WS-INFIL2-STATUS for spaces or '00'; if not, displays error and performs 9999-ABEND. No data transformations or business logic beyond status validation; inputs are system date functions, outputs are open files and display messages. Error handling is immediate abend on open failure to prevent processing with invalid files. No subordinate calls. It ensures files are ready before main processing loops.

### 2000-READ-ROOT-SEG-FILE
This paragraph reads the next root segment record from INFILE1 into INFIL1-REC and handles end-of-file or errors. It performs a sequential READ INFILE1, then checks WS-INFIL1-STATUS: if spaces or '00', moves INFIL1-REC to PENDING-AUTH-SUMMARY and performs 2100-INSERT-ROOT-SEG to load into IMS. If status '10', sets END-ROOT-SEG-FILE to 'Y' to exit the loop in MAIN-PARA. Other statuses trigger error display but no abend here (abend deferred to insert). Inputs consumed: INFILE1 records; outputs: updated EOF flag or data moved to summary segment for insert. No business decisions beyond status checks; error handling logs but continues to insert which may abend. Called repeatedly in loop until EOF.

### 2100-INSERT-ROOT-SEG
This paragraph inserts a root segment (PAUTSUM0) into IMS using unqualified SSA. It issues CALL 'CBLTDLI' with FUNC-ISRT, passing PAUTBPCB, PENDING-AUTH-SUMMARY (from file read), and ROOT-UNQUAL-SSA. Displays decorative messages and checks PAUT-PCB-STATUS post-call: spaces indicates success (display confirm), 'II' duplicate (display note), any other triggers display failure with status and performs 9999-ABEND. Inputs: pre-moved PENDING-AUTH-SUMMARY; outputs: IMS root segment or abend. Business logic enforces no duplicates but skips them without counting; error handling abends on insert failures to maintain DB integrity. No further calls.

### 3000-READ-CHILD-SEG-FILE
This paragraph reads the next child segment record from INFILE2 into INFIL2-REC and selectively processes based on key validity. It performs READ INFILE2, checks WS-INFIL2-STATUS: if spaces/'00', tests if ROOT-SEG-KEY IS NUMERIC; if yes, moves ROOT-SEG-KEY to QUAL-SSA-KEY-VALUE, CHILD-SEG-REC to PENDING-AUTH-DETAILS, and performs 3100-INSERT-CHILD-SEG. If status '10', sets END-CHILD-SEG-FILE 'Y' for loop exit; other statuses display error (no abend here). Inputs: INFILE2 records; outputs: EOF flag or prepared SSA/data for child insert. Business logic skips non-numeric keys (invalid data); error handling defers to child insert. Called in loop post-root processing.

### 3100-INSERT-CHILD-SEG
This paragraph positions to the parent root segment via GU before child insert to enforce hierarchy. It initializes PAUT-PCB-STATUS, issues CALL 'CBLTDLI' FUNC-GU with PAUTBPCB, PENDING-AUTH-SUMMARY (placeholder), ROOT-QUAL-SSA (keyed by ROOT-SEG-KEY). Displays messages, checks PAUT-PCB-STATUS: if spaces, displays success and performs 3200-INSERT-IMS-CALL; if not spaces or 'II', displays failure details (status, KEYFB) and abends via 9999-ABEND. Inputs: qualified SSA from child key, summary layout; outputs: positioned PCB for child insert or abend. Business logic requires parent existence for child insert; error handling abends on GU failure (e.g., root not found).

### 3200-INSERT-IMS-CALL
This paragraph performs the actual IMS insert for the child segment (PAUTDTL1) under the positioned parent. It issues CALL 'CBLTDLI' FUNC-ISRT with PAUTBPCB, PENDING-AUTH-DETAILS (from file), CHILD-UNQUAL-SSA. Checks PAUT-PCB-STATUS: spaces success (display), 'II' duplicate (display), other displays failure (status, KEYFB) and abends via 9999-ABEND. Inputs: pre-GU positioned PCB and child data; outputs: inserted child segment or abend. No additional logic/decisions; error handling mirrors root insert to skip duplicates but fail others.

### 4000-FILE-CLOSE
This termination paragraph closes input files after all processing. It displays 'CLOSING THE FILE', performs CLOSE INFILE1 and checks WS-INFIL1-STATUS (spaces/'00' continue, else display error); repeats for INFILE2. Inputs: open files; outputs: closed files, potential error displays (no abend). No business logic or validations beyond status; error handling logs close failures but allows GOBACK. Ensures clean file shutdown.

### 9999-ABEND
This error handler abends the program on critical failures (file I/O, IMS calls). It displays 'IMS LOAD ABENDING ...', sets RETURN-CODE to 16, and GOBACKs. Inputs: none (called on error); outputs: abend with RC 16. No recovery or logging beyond display; terminates immediately to prevent partial loads.

## Open Questions

- ? Contents and field layouts of copybooks CIPAUSMY, CIPAUDTY, PAUTBPCB, IMSFUNCS?
  - Context: Layouts referenced but not provided; assumed standard IMS segment/PCB but fields like PA-AUTH-DATE-9C (commented) unknown
- ? Purpose of many unused WS variables (e.g., WS-NO-CHKP, WS-EXPIRY-DAYS, PRM-INFO, counters)?
  - Context: Defined in WS but never referenced or incremented
- ? Assumptions on input file sorting/order? Separate root/child loops imply roots loaded first, but GU per child allows flexibility
  - Context: Code processes all roots then all children; potential for missing roots if not all in INFILE1
- ? Invocation context (JCL, calling program)? ENTRY 'DLITCBL' suggests subprogram call
  - Context: Uses PAUTBPCB linkage, but called_by unknown
