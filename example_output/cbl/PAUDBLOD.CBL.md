# PAUDBLOD

**File**: `cbl/PAUDBLOD.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 15:16:42.514450

## Purpose

PAUDBLOD is a batch IMS database loader that reads root segment records (PAUTSUM0 pending authorization summaries) from INFILE1 and inserts them into the IMS database using ISRT calls. It then reads child segment records (PAUTDTL1 pending authorization details) from INFILE2, qualifies the corresponding root segment via GU using the root key, and inserts the child segments using ISRT. Duplicate segments (status 'II') are skipped with a display message, while other IMS errors cause abend.

**Business Context**: Supports loading of pending authorization summary and detail data into IMS database PAUT for financial or authorization processing workflows

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | IOType.FILE_SEQUENTIAL | Sequential file containing root segment records (PAUTSUM0) in 100-byte fixed format mapped to PENDING-AUTH-SUMMARY |
| INFILE2 | IOType.FILE_SEQUENTIAL | Sequential file containing child segment records prefixed with 11-digit COMP-3 root key (PAUTDTL1) mapped to PENDING-AUTH-DETAILS |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | IMS root segment (Pending Authorization Summary) loaded via unqualified ISRT |
| PAUTDTL1 | IOType.IMS_SEGMENT | IMS child segment (Pending Authorization Details) loaded via unqualified ISRT under qualified parent root |

## Business Rules

- **BR001**: Child segments are only processed if the ROOT-SEG-KEY field is numeric
- **BR002**: Duplicate segments (IMS status 'II') are tolerated and skipped without abending
- **BR003**: All root segments are loaded before any child segments to ensure parent existence

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph controlling the entire program flow. It first executes an ENTRY 'DLITCBL' USING PAUTBPCB to support alternate entry (line 171). It displays a startup message (173). It then performs 1000-INITIALIZE THRU 1000-EXIT to accept dates, display them, and open both input files with error abend checks (175). Next, it enters a loop performing 2000-READ-ROOT-SEG-FILE THRU 2000-EXIT until END-ROOT-SEG-FILE = 'Y', reading and inserting all root segments from INFILE1 (177-178). After exhausting roots, it performs 3000-READ-CHILD-SEG-FILE THRU 3000-EXIT until END-CHILD-SEG-FILE = 'Y', processing all child segments from INFILE2 with parent qualification (180-181). It then performs 4000-FILE-CLOSE THRU 4000-EXIT to close files with status checks (183). No direct data consumption or production; delegates to subordinates. Error handling is propagated from called paragraphs via abend. Finally, GOBACK to caller (187).

### 1000-INITIALIZE
This initialization paragraph sets up program variables and opens input files. It accepts CURRENT-DATE from DATE and CURRENT-YYDDD from DAY (193-194). It displays formatted date information (196-198). It opens INFILE1 and checks WS-INFIL1-STATUS; if not SPACES or '00', displays error and performs 9999-ABEND (201-207). Similarly opens INFILE2 and checks WS-INFIL2-STATUS with identical error handling (209-215). No data transformations or business logic beyond status validation. Consumes no prior data; produces open files ready for reads. Error handling is explicit with abend on open failures to prevent processing with invalid files. No calls to other paragraphs. Control passes to 1000-EXIT.

### 2000-READ-ROOT-SEG-FILE
This paragraph reads the next root segment record from INFILE1 in the main root loading loop. It performs READ INFILE1 (226). If WS-INFIL1-STATUS = SPACES or '00', moves INFIL1-REC to PENDING-AUTH-SUMMARY and performs 2100-INSERT-ROOT-SEG THRU 2100-EXIT to load into IMS (228-230). If status = '10', sets END-ROOT-SEG-FILE = 'Y' to exit loop (232-233). For other statuses, displays error message but does not abend, allowing continuation (235). Consumes records from opened INFILE1; produces populated PENDING-AUTH-SUMMARY for insert. No complex business logic; simple status-based decisions. Error handling lenient on reads (display only). Calls 2100 for successful reads. Control to 2000-EXIT.

### 2100-INSERT-ROOT-SEG
This paragraph inserts a root segment into IMS using unqualified ISRT. It calls 'CBLTDLI' USING FUNC-ISRT, PAUTBPCB, PENDING-AUTH-SUMMARY, ROOT-UNQUAL-SSA (244-247). Displays decorative messages (248,252). If PAUT-PCB-STATUS = SPACES, displays success (253-254). If 'II', displays duplicate message (256-257). If neither, displays failure status and performs 9999-ABEND (259-261). Consumes pre-mapped PENDING-AUTH-SUMMARY from read; produces IMS PAUTSUM0 segment. Business logic enforces uniqueness tolerating duplicates. Strict error handling abends on unexpected IMS statuses. No subordinate calls. Control to 2100-EXIT.

### 3000-READ-CHILD-SEG-FILE
This paragraph reads the next child segment record from INFILE2 in the main child loading loop. It performs READ INFILE2 (272). If WS-INFIL2-STATUS = SPACES or '00', checks if ROOT-SEG-KEY IS NUMERIC; if yes, moves ROOT-SEG-KEY to QUAL-SSA-KEY-VALUE, CHILD-SEG-REC to PENDING-AUTH-DETAILS, and performs 3100-INSERT-CHILD-SEG THRU 3100-EXIT (274-282). If status = '10', sets END-CHILD-SEG-FILE = 'Y' to exit loop (284-285). For other statuses, displays error but does not abend (287). Consumes records from INFILE2; produces populated SSA and details for child insert. Key business logic skips non-numeric root keys. Error handling lenient on reads. Calls 3100 for valid records. Control to 3000-EXIT.

### 3100-INSERT-CHILD-SEG
This paragraph performs GU on parent root before child insert to establish parentage. Initializes PAUT-PCB-STATUS (295). Calls 'CBLTDLI' USING FUNC-GU, PAUTBPCB, PENDING-AUTH-SUMMARY, ROOT-QUAL-SSA (296-299). Displays messages (300,304). If PAUT-PCB-STATUS = SPACES, displays success and performs 3200-INSERT-IMS-CALL THRU 3200-EXIT (305-309). If not SPACES or 'II', displays failure, key feedback, and abends (310-313). Consumes qualified SSA and details from read; temporarily retrieves root into PENDING-AUTH-SUMMARY for parentage. Ensures parent exists before child insert. Strict error handling abends on GU failure. Calls 3200 for success. Control to 3100-EXIT.

### 3200-INSERT-IMS-CALL
This paragraph inserts the child segment into IMS using unqualified ISRT after successful parent GU. Calls 'CBLTDLI' USING FUNC-ISRT, PAUTBPCB, PENDING-AUTH-DETAILS, CHILD-UNQUAL-SSA (321-324). If PAUT-PCB-STATUS = SPACES, displays success (326-327). If 'II', displays duplicate (329-330). If neither, displays failure, key feedback, and abends (332-335). Consumes pre-mapped PENDING-AUTH-DETAILS; produces IMS PAUTDTL1 segment under located parent. Tolerates duplicates per business rule. Strict error handling on insert failures. No further calls. Control to 3200-EXIT.

### 4000-FILE-CLOSE
This termination paragraph closes input files after all processing. Displays closing message (342). Closes INFILE1 and checks WS-INFIL1-STATUS; displays error if not SPACES/'00' but no abend (343-349). Closes INFILE2 similarly with display-only error handling (350-356). Consumes no data; ensures files closed. No business logic or validations. Lenient error handling (display only). No calls. Control to 4000-EXIT.

### 9999-ABEND
This error termination paragraph abends the program on fatal errors. Displays abend message (363). Sets RETURN-CODE to 16 and GOBACK (365-366). Consumes no inputs; produces non-zero return code. Used by open and IMS call failures. No conditions checked here; invoked explicitly. No subordinate calls. Control to 9999-EXIT (unused).

## Open Questions

- ? Purpose of unused counters (e.g., WS-NO-CHKP, WS-AUTH-SMRY-PROC-CNT, WS-NO-SUMRY-DELETED)
  - Context: Defined in WS-VARIABLES (63-69) but never incremented or displayed
- ? Usage of PRM-INFO parameters (P-EXPIRY-DAYS, P-CHKP-FREQ, etc.)
  - Context: Defined (129-139) but never populated via ACCEPT or LINKAGE
- ? Role of ENTRY 'DLITCBL' USING PAUTBPCB
  - Context: Executed at start (171) but unclear if program is dynamically called under this name
