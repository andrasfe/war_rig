# PAUDBLOD

**File**: `cbl/PAUDBLOD.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 17:37:52.345074

## Purpose

PAUDBLOD is a batch IMS database loader utility that processes sequential input files to insert hierarchical segments into the PAUT IMS database. It reads root segments (PAUTSUM0) from INFILE1 and inserts them using unqualified ISRT calls, tolerating duplicates ('II' status). It then reads child segments (PAUTDTL1) from INFILE2, each prefixed with the parent root key, performs a qualified GU on the parent root to establish parentage, and inserts the child using unqualified ISRT.

**Business Context**: Loads pending authorization summary and detail data into IMS for authorization processing, using account ID (ACCNTID) as key.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | IOType.FILE_SEQUENTIAL | Fixed 100-byte records containing PAUTSUM0 root segment data |
| INFILE2 | IOType.FILE_SEQUENTIAL | Records with S9(11) COMP-3 root key followed by 200-byte PAUTDTL1 child segment data |
| PAUTBPCB | IOType.OTHER | IMS PCB mask providing database access and status feedback |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Pending authorization summary root segments inserted into IMS database |
| PAUTDTL1 | IOType.IMS_SEGMENT | Pending authorization details child segments inserted under PAUTSUM0 parents |

## Business Rules

- **BR001**: Insert root segments unconditionally, skipping duplicates without abend
- **BR002**: For child insertion, parent root must exist and be retrievable via qualified GU
- **BR003**: Process child records only if ROOT-SEG-KEY is numeric
- **BR004**: Abort program on any IMS status other than spaces or 'II', or file I/O errors

## Paragraphs/Procedures

### MAIN-PARA
MAIN-PARA is the primary orchestration point controlling the overall IMS load process flow. It defines an alternate entry point 'DLITCBL' using the PAUTBPCB linkage for potential dynamic invocation (171). Displays a startup message upon entry (173). Performs 1000-INITIALIZE to accept system dates and open input files INFILE1 (root data) and INFILE2 (child data) (175). Enters a loop invoking 2000-READ-ROOT-SEG-FILE repeatedly until END-ROOT-SEG-FILE flag is set on EOF from INFILE1 (177-178). After root loading completes, enters another loop calling 3000-READ-CHILD-SEG-FILE until END-CHILD-SEG-FILE on EOF from INFILE2 (180-181). Upon loop exits, performs 4000-FILE-CLOSE to release files (183). Issues GOBACK to return control to invoker (187). Performs no direct data reads, writes, or validations; delegates all logic and error handling to subordinate paragraphs. Relies on ABEND in children for fatal errors.

### 1000-INITIALIZE
This initialization paragraph sets up runtime variables and opens input files for processing. It accepts CURRENT-DATE from DATE and CURRENT-YYDDD from DAY for potential logging/timestamping (193-194). Displays formatted date information for audit trail (196-198). Opens INFILE1 INPUT mode and validates WS-INFIL1-STATUS equals spaces or '00'; if invalid, displays error message and branches to 9999-ABEND (201-207). Similarly opens INFILE2 and performs identical status check and error handling (209-215). Consumes no input data records; reads only system date. Produces display output for diagnostics and sets file status flags. Implements file-open validation business logic with immediate termination on failure. Calls no other paragraphs except potential ABEND. Exits cleanly to return control to caller (218).

### 2000-READ-ROOT-SEG-FILE
This paragraph reads and processes one root segment record from INFILE1 in the main loop. Performs READ INFILE1 into INFIL1-REC (226). Checks WS-INFIL1-STATUS; if spaces or '00', MOVEs record to PENDING-AUTH-SUMMARY I/O area and performs 2100-INSERT-ROOT-SEG to load into IMS (228-230). If status '10', sets END-ROOT-SEG-FILE to 'Y' signaling loop exit (232-233). For other statuses, displays read error but does not ABEND here (235). Consumes INFIL1-REC from file. Produces updated PENDING-AUTH-SUMMARY for IMS insert via subordinate. Implements EOF detection and basic read validation logic. Delegates insert logic and errors to 2100. No direct writes or further calls.

### 2100-INSERT-ROOT-SEG
This paragraph inserts a single root segment into IMS PAUTSUM0 using unqualified SSA. Issues CALL 'CBLTDLI' ISRT with PAUTBPCB, PENDING-AUTH-SUMMARY data, and ROOT-UNQUAL-SSA (244-247). Displays decorative messages around call (248,252). If PAUT-PCB-STATUS = spaces, displays success (253-254). If 'II', displays duplicate message without abend (256-257). For any other status, displays failure with code and performs 9999-ABEND (259-261). Consumes pre-loaded PENDING-AUTH-SUMMARY from caller. Writes segment to IMS or tolerates duplicate. Implements IMS insert validation and duplicate tolerance business rule. No further calls except ABEND; relies on PCB status for decisions.

### 3000-READ-CHILD-SEG-FILE
This paragraph reads and conditionally processes one child segment from INFILE2 in the main loop. Performs READ INFILE2 into INFIL2-REC (272). If WS-INFIL2-STATUS spaces or '00', tests if ROOT-SEG-KEY IS NUMERIC (274-275). If numeric, MOVEs ROOT-SEG-KEY to QUAL-SSA-KEY-VALUE, CHILD-SEG-REC to PENDING-AUTH-DETAILS, and performs 3100-INSERT-CHILD-SEG (277,280-281). If status '10', sets END-CHILD-SEG-FILE 'Y' for loop exit (284-285). Other statuses display error without abend (287). Consumes INFIL2-REC fields. Prepares data for child insert via subordinate. Implements key validity check (numeric) as business rule before processing. Delegates full insert logic to 3100.

### 3100-INSERT-CHILD-SEG
This paragraph handles IMS insert of child segment under specific parent root. INITIALIZEs PAUT-PCB-STATUS (295). Performs CALL 'CBLTDLI' GU on PAUTSUM0 using qualified ROOT-QUAL-SSA with key from input (296-299). Displays around call (300,304). If PAUT-PCB-STATUS = spaces, displays success and performs 3200-INSERT-IMS-CALL to insert child (305-309). Regardless of prior, if status NOT spaces AND NOT 'II', displays failure details (PCB status, KEYFB) and ABENDs (310-313). Consumes PENDING-AUTH-DETAILS and SSA key from caller. Retrieves parent via GU for parentage; writes child only if parent found. Implements hierarchical insert logic with parent existence check. Calls 3200 only on successful GU.

### 3200-INSERT-IMS-CALL
This paragraph performs the actual IMS ISRT for child segment after parent GU success. Issues CALL 'CBLTDLI' ISRT using PAUTBPCB, PENDING-AUTH-DETAILS, and CHILD-UNQUAL-SSA (321-324). If PAUT-PCB-STATUS = spaces, displays insert success (326-327). If 'II', displays duplicate message (329-330). For other statuses, displays failure with details (KEYFB) and ABENDs (332-335). Consumes pre-loaded child data and PCB from prior GU. Writes PAUTDTL1 segment to IMS under current parent. Implements duplicate tolerance similar to root insert. No further subordinate calls except ABEND. Relies on PCB for post-insert validation.

### 4000-FILE-CLOSE
This termination paragraph closes input files after processing completes. Displays close message (342). Performs CLOSE INFILE1 and checks WS-INFIL1-STATUS; displays error if invalid but no abend (343-349). Similarly CLOSE INFILE2 and status check (350-356). Consumes open file handles from initialization. Produces no data outputs; only diagnostic displays on close errors. Implements graceful file shutdown validation without fatal errors. No business decisions or data transforms. Calls no other paragraphs.

### 9999-ABEND
This error termination paragraph handles all fatal abends across the program. Displays 'IMS LOAD ABENDING ...' message (363). Sets RETURN-CODE to 16 (365). Performs GOBACK with non-zero code (366). Consumed from any error context via PERFORM. Produces standardized abend message and return code for JCL/IMS monitoring. Implements uniform fatal error handling policy. No validation, transforms, or further calls. Invoked on file I/O failures, IMS errors beyond duplicates.

## Open Questions

- ? Exact field layouts and key structures in CIPAUSMY, CIPAUDTY, PAUTBPCB
  - Context: Copybooks referenced but source not provided; limits field-level data flow detail
- ? Usage of unused variables (e.g., WS-EXPIRY-DAYS, PRM-INFO, counters like WS-NO-CHKP)
  - Context: Defined in WS but no references in procedure code; possible dead code or partial source
- ? Invocation details (JCL, PARM usage, DLITCBL entry context)
  - Context: No PARM acceptance; ENTRY 'DLITCBL' suggests IMS DL/I batch or subprogram use
