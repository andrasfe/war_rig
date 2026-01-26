# PAUDBUNL

**File**: `cbl/PAUDBUNL.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 15:14:37.991980

## Purpose

This IMS database unload program reads root segments named PAUTSUM0 (Pending Authorization Summary) from the PAUT database using unqualified GN calls and writes them to sequential file OPFILE1 if the PA-ACCT-ID is numeric. For each valid root segment, it reads dependent child segments PAUTDTL1 (Pending Authorization Details) using GNP calls and writes them to OPFILE2, prefixed with the root's PA-ACCT-ID as a packed key. Processing continues until the end of the database is reached (status 'GB'), with counters tracking records processed and errors causing abend.

**Business Context**: Supports unloading of pending authorization data from IMS for archiving, migration, reporting, or offline batch processing in a financial or authorization system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUT IMS Database - PAUTSUM0 | IOType.IMS_SEGMENT | Root segments containing Pending Authorization Summary data |
| PAUT IMS Database - PAUTDTL1 | IOType.IMS_SEGMENT | Child segments containing Pending Authorization Details data, dependent on root PAUTSUM0 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OPFILE1 | IOType.FILE_SEQUENTIAL | Sequential file containing copies of root PAUTSUM0 segments (PENDING-AUTH-SUMMARY records) |
| OPFILE2 | IOType.FILE_SEQUENTIAL | Sequential file containing root PA-ACCT-ID as packed key followed by child PAUTDTL1 segments (PENDING-AUTH-DETAILS records) |

## Business Rules

- **BR001**: Root summary records and their child details are only written to output files if the PA-ACCT-ID field is numeric

## Paragraphs/Procedures

### MAIN-PARA
This paragraph serves as the primary entry and orchestration point for the entire program flow. It defines an alternate ENTRY point named 'DLITCBL' that accepts the PAUTBPCB linkage structure for IMS PCB access (line 158). It begins by performing 1000-INITIALIZE to accept system dates, display startup information, and open output files OPFILE1 and OPFILE2, consuming no data but producing opened files and console messages. Following initialization, it enters a main processing loop that repeatedly performs 2000-FIND-NEXT-AUTH-SUMMARY until WS-END-OF-ROOT-SEG is set to 'Y', reading and unloading all root and child segments (lines 163-164). No direct business logic or decisions are made here; flow control relies on flags set by subordinate paragraphs. After the loop completes, it performs 4000-FILE-CLOSE to shut down output files (line 166). Error handling is delegated to called paragraphs, which abend on failures. It calls no external programs, only internal paragraphs for structured processing. Upon completion, it executes GOBACK to return to the caller with the final return code (line 170). Overall, it ensures sequential unload of the entire database hierarchy.

### 1000-INITIALIZE
This paragraph is responsible for program initialization, preparing environment and resources before main processing. It consumes system date information by ACCEPTing CURRENT-DATE from DATE and CURRENT-YYDDD from DAY (lines 176-177). It produces console DISPLAY messages showing program start, date, and banners (lines 180-183). It then opens OPFILE1 as OUTPUT, checks WS-OUTFL1-STATUS for '00' or spaces, and abends via 9999-ABEND if invalid (lines 186-192). Similarly, it opens OPFILE2 and validates WS-OUTFL2-STATUS, abending on error (lines 194-200). No business logic or conditions beyond file status checks; validation is strict to ensure output readiness. No subordinate paragraphs or programs are called. Error handling is inline with DISPLAY of status and abend call. It initializes no working variables directly but relies on prior compiler defaults. Control passes to 1000-EXIT upon success.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph handles reading the next root segment (PAUTSUM0) from the IMS database and orchestrates writing it along with its children. It consumes IMS data via CALL 'CBLTDLI' using FUNC-GN, PAUTBPCB, PENDING-AUTH-SUMMARY IO-area, and ROOT-UNQUAL-SSA (lines 213-216). On success (PAUT-PCB-STATUS spaces), it increments read counters WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT (lines 225-226), moves the segment to OPFIL1-REC (227), initializes ROOT-SEG-KEY and CHILD-SEG-REC (228-229), and moves PA-ACCT-ID to ROOT-SEG-KEY (230). It then checks if PA-ACCT-ID IS NUMERIC; if yes, writes OPFIL1-REC (233) and performs 3000-FIND-NEXT-AUTH-DTL loop until WS-END-OF-CHILD-SEG='Y' to unload children (235-237). Business logic enforces numeric PA-ACCT-ID for processing to avoid invalid data writes. On 'GB' status, sets END-OF-AUTHDB and WS-END-OF-ROOT-SEG='Y' to exit loop (239-242). Other statuses trigger DISPLAY of error/KEYFB and 9999-ABEND (243-246). It calls IMS interface and subordinate 3000 paragraph for children. PCB status is initialized before call (212).

### 3000-FIND-NEXT-AUTH-DTL
This paragraph reads the next child segment (PAUTDTL1) dependent on the current root using GNP call. It consumes IMS data via CALL 'CBLTDLI' with FUNC-GNP, PAUTBPCB, PENDING-AUTH-DETAILS IO-area, and CHILD-UNQUAL-SSA (lines 257-260). On success (spaces), increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT counters (268-269; note potential counter mismatch for details), moves PENDING-AUTH-DETAILS to CHILD-SEG-REC (270), and writes OPFIL2-REC (271). Business logic is minimal, assuming valid parentage from GNP. On 'GE' status, sets WS-END-OF-CHILD-SEG='Y' to exit child loop and displays flag (273-277). Other statuses display error/KEYFB and perform 9999-ABEND (279-282). No further conditions or validations like numeric checks. It initializes PAUT-PCB-STATUS post-call (284). Called repeatedly from 2000 until end of children for current root. Produces child records in OPFILE2 keyed by root PA-ACCT-ID.

### 4000-FILE-CLOSE
This paragraph performs cleanup by closing output files after all data unload. It consumes the opened OPFILE1 and OPFILE2, displaying 'CLOSING THE FILE' (290). It closes OPFILE1 and checks WS-OUTFL1-STATUS for '00'/spaces, displaying error if invalid but no abend (293-297). Similarly closes OPFILE2 and checks WS-OUTFL2-STATUS, displaying error if bad (298-304). No inputs or business logic; purely resource release. No decisions beyond status display. No calls to other paragraphs or programs. Error handling is soft (display only, no abend). Ensures files are properly closed before program exit. Control passes to 4000-EXIT.

### 9999-ABEND
This paragraph handles all program abends due to errors. It consumes no specific data but is triggered by file/IMS failures. It displays 'IMSUNLOD ABENDING ...' message (311). Sets RETURN-CODE to 16 (313) and executes GOBACK (314) to terminate abnormally. No business logic, inputs, or outputs beyond message and RC. No conditions checked here. Called from initialization opens (190,199), root IMS call (246), child IMS call (282). No subordinate calls. Provides consistent abend path with RC=16 across error scenarios.

## Open Questions

- ? Exact field layouts and contents of copybooks CIPAUSMY, CIPAUDTY, PAUTBPCB, and IMSFUNCS
  - Context: Copybooks are referenced but source not provided, preventing field-level data flow details
- ? Purpose and usage of counters like WS-NO-SUMRY-READ in child processing (lines 268-269)
  - Context: Counters named for summary are incremented in detail processing, possible copy-paste error or misnomer
- ? What calls this program (e.g., JCL or parent program using ENTRY 'DLITCBL')
  - Context: No calling context in source; ENTRY suggests dynamic linkage
