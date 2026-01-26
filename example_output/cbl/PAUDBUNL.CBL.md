# PAUDBUNL

**File**: `cbl/PAUDBUNL.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 14:21:10.930335

## Purpose

This batch COBOL program unloads data from an IMS database by reading root segments (PAUTSUM0 Pending Authorization Summary) using GN calls and writing them to sequential file OPFILE1. For each root segment, it then reads child segments (PAUTDTL1 Pending Authorization Details) using GNP calls and writes them prepended with the root segment key to OPFILE2. The process continues until the end of the database (GB status) is reached, with counters tracking records processed.

**Business Context**: Supports unloading of pending authorization data from IMS database PAUT for potential archiving, migration, or offline processing.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUT IMS Database | IOType.IMS_SEGMENT | Root segments (PAUTSUM0 via PENDING-AUTH-SUMMARY) and child segments (PAUTDTL1 via PENDING-AUTH-DETAILS) read via DL/I calls using PAUTBPCB |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OPFILE1 | IOType.FILE_SEQUENTIAL | Contains root segments (PAUTSUM0) copied from PENDING-AUTH-SUMMARY as fixed 100-byte records |
| OPFILE2 | IOType.FILE_SEQUENTIAL | Contains child segments (PAUTDTL1) prepended with root segment key (PA-ACCT-ID as ROOT-SEG-KEY) as records with 11-byte COMP-3 key + 200-byte data |

## Business Rules

- **BR001**: Only write root segment to OPFILE1 if PA-ACCT-ID is numeric

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph for the entire IMS unload process, establishing the DLITCBL entry convention for batch DL/I execution. It consumes the PAUTBPCB linkage area passed from IMS control region. It first performs initialization to open output files and set up dates/displays. Then it enters a loop performing 2000-FIND-NEXT-AUTH-SUMMARY for each root segment until WS-END-OF-ROOT-SEG is 'Y' (end of database). Within each 2000 iteration, child segments are processed via 3000 loops. After all roots are processed, it performs 4000-FILE-CLOSE to shut down files. No direct business decisions are made here; flow control relies on flags set by subordinate paragraphs. Errors in subordinates propagate via abend calls. Counters like WS-NO-SUMRY-READ are indirectly updated via performs. Finally, it GOBACKs with return code set only if abended.

### 1000-INITIALIZE
This paragraph handles program startup by accepting system dates into CURRENT-DATE and CURRENT-YYDDD, displaying startup messages including program name and date. It consumes no input files or data beyond system date. It produces initialized working storage variables and opens output files OPFILE1 and OPFILE2. Business logic includes validating file status after OPEN: if not spaces or '00', displays error and performs 9999-ABEND. No other decisions or validations. It calls no other paragraphs but is invoked from MAIN-PARA. Error handling is explicit for file opens, leading to immediate abend on failure. Displays provide audit trail. Exits cleanly to allow main loop.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph reads the next root segment (PAUTSUM0) from IMS PAUT database using GN call via CBLTDLI with ROOT-UNQUAL-SSA. It consumes PAUTBPCB and prior PCB state, reading into PENDING-AUTH-SUMMARY. On success (spaces status), increments counters WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves segment to OPFIL1-REC, initializes ROOT-SEG-KEY and CHILD-SEG-REC, moves PA-ACCT-ID to ROOT-SEG-KEY, checks if PA-ACCT-ID numeric before WRITE to OPFILE1, then loops PERFORM 3000-FIND-NEXT-AUTH-DTL until WS-END-OF-CHILD-SEG='Y'. If PCB status 'GB', sets end flags for root and database. If other error statuses, displays status/KEYFB and abends. Initializes PAUT-PCB-STATUS before call. Business logic enforces numeric account ID for root write; controls loop over children per root.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph reads the next child segment (PAUTDTL1) under current root using GNP call via CBLTDLI with CHILD-UNQUAL-SSA. It consumes PAUTBPCB with positioned root context, reading into PENDING-AUTH-DETAILS. On success (spaces), sets MORE-AUTHS flag (unused), increments WS-NO-DTL-READ (wait, code has WS-NO-SUMRY-READ? likely typo for details), moves to CHILD-SEG-REC (pre-set with root key), and WRITEs OPFIL2-REC. If 'GE' (segment not found), sets WS-END-OF-CHILD-SEG='Y' and displays flag. If other statuses, displays error/KEYFB and abends. Initializes PAUT-PCB-STATUS post-call. No validations beyond status; assumes root key already set. Error handling abends on non-spaces/GE. Called repeatedly from 2000 until child end.

### 4000-FILE-CLOSE
This paragraph performs program shutdown by closing output files OPFILE1 and OPFILE2. It consumes open file handles and statuses. It produces closed files and displays closure messages. Business logic checks WS-OUTFL*-STATUS after each CLOSE: if not spaces/'00', displays error but does not abend (non-fatal). No decisions or validations beyond status. Calls no others. Error handling is display-only. Displays 'CLOSING THE FILE' for audit. Invoked once from MAIN-PARA after all processing.

### 9999-ABEND
This error termination paragraph is called on any fatal condition like file open/close failures or IMS call errors. It consumes no specific inputs but relies on prior DISPLAYs for context. It produces a final display 'IMSUNLOD ABENDING ...' (mismatch with program ID), sets RETURN-CODE to 16, and GOBACKs. No business logic or conditions checked here. No validations. Calls nothing. Handles all abend paths uniformly without recovery. Note: 9999-EXIT defined but unused.

## Open Questions

- ? Exact field layouts and contents of copybooks CIPAUSMY, CIPAUDTY, PAUTBPCB, IMSFUNCS
  - Context: Source includes COPY statements but not inline definitions; fields like PA-ACCT-ID referenced without visible structure
- ? Purpose of counters like WS-NO-CHKP, WS-NO-SUMRY-DELETED, WS-NO-DTL-DELETED (initialized but not incremented/used)
  - Context: Defined in WS-VARIABLES but no PERFORMs or logic reference them in visible code
- ? IMS PSB name and full PCB details
  - Context: WS-PGMNAME 'IMSUNLOD' and commented PSB-NAME; PCB from copybook
- ? Expiry date logic (WS-AUTH-DATE, WS-EXPIRY-DAYS, WS-DAY-DIFF)
  - Context: Variables defined and PRM-INFO has P-EXPIRY-DAYS but ACCEPT commented; no usage in procedure code
