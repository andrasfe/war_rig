# PAUDBUNL

**File**: `cbl/PAUDBUNL.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 02:30:46.356586

## Purpose

This utility program unloads root segments (PAUTSUM0 Pending Authorization Summary) from an IMS database to sequential file OPFILE1 and associated child segments (PAUTDTL1 Pending Authorization Details) prefixed with the root segment's PA-ACCT-ID key to OPFILE2. It uses unqualified SSAs for sequential GN calls on roots and GNP calls on children under each root until end-of-database or end-of-children conditions are met. Files are opened at start, written sequentially, and closed at end, with abend on I/O or IMS errors.

**Business Context**: Supports extraction of pending authorization data from IMS PAUT database for backup, migration, reporting, or offline processing.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUT Database (PAUTSUM0 root segments) | IOType.IMS_SEGMENT | Pending Authorization Summary root segments read via IMS GN call using ROOT-UNQUAL-SSA |
| PAUT Database (PAUTDTL1 child segments) | IOType.IMS_SEGMENT | Pending Authorization Details child segments under current root, read via IMS GNP call using CHILD-UNQUAL-SSA |
| PAUTBPCB | IOType.OTHER | IMS PCB mask passed via linkage for database access |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OPFILE1 | IOType.FILE_SEQUENTIAL | Contains copied root Pending Authorization Summary segments (PIC X(100)) |
| OPFILE2 | IOType.FILE_SEQUENTIAL | Contains root segment key (PA-ACCT-ID as ROOT-SEG-KEY PIC S9(11) COMP-3) followed by copied child Pending Authorization Details segments (PIC X(200)) |

## Business Rules

- **BR001**: Root summary record is only written to OPFILE1 if PA-ACCT-ID is numeric

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point paragraph that orchestrates the entire IMS unload process. It accepts the PAUTBPCB linkage area for IMS database access. It first performs 1000-INITIALIZE to accept dates, display startup info, and open output files OPFILE1 and OPFILE2. Then it enters a loop performing 2000-FIND-NEXT-AUTH-SUMMARY until WS-END-OF-ROOT-SEG is 'Y', which handles reading each root summary segment, writing it to OPFILE1, and nested loop for child details to OPFILE2. Upon loop exit (end of database), it performs 4000-FILE-CLOSE to shut down files safely. No explicit error handling here beyond subordinate calls, but abends propagate via 9999-ABEND. It calls no external programs, only internal paragraphs. Counters like WS-NO-SUMRY-READ are incremented in subordinates but not displayed or used further here.

### 1000-INITIALIZE
This paragraph handles program startup initialization including date acceptance and output file opens. It consumes system date via ACCEPT CURRENT-DATE FROM DATE and CURRENT-YYDDD FROM DAY, then displays program start message and date. It opens OPFILE1 and checks WS-OUTFL1-STATUS for spaces or '00'; if not, displays error and performs 9999-ABEND. Similarly opens OPFILE2 and checks WS-OUTFL2-STATUS with same error handling. No data transformations or business decisions beyond status validation. It produces open files ready for writes and sets up WS file status flags. Calls no other paragraphs or programs. Error handling is immediate abend on open failure to prevent processing with bad files.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph performs the IMS GN call to retrieve the next unqualified root PAUTSUM0 segment and coordinates child processing. It initializes PAUT-PCB-STATUS, calls CBLTDLI with FUNC-GN, PAUTBPCB, PENDING-AUTH-SUMMARY IO-area, and ROOT-UNQUAL-SSA. If PAUT-PCB-STATUS is spaces (success), increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves segment to OPFIL1-REC, clears output fields, moves PA-ACCT-ID to ROOT-SEG-KEY, and if PA-ACCT-ID numeric, writes OPFILE1-REC then loops 3000-FIND-NEXT-AUTH-DTL until WS-END-OF-CHILD-SEG='Y'. If 'GB' (end of DB), sets END-OF-AUTHDB and WS-END-OF-ROOT-SEG='Y'. If other status, displays error/KEYFB and abends via 9999-ABEND. Business logic enforces numeric check on account ID before root write. It reads from IMS root, writes to OPFILE1, and triggers child reads/writes.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph performs IMS GNP call to retrieve the next child PAUTDTL1 segment under the current root and writes it to OPFILE2. It calls CBLTDLI with FUNC-GNP, PAUTBPCB, PENDING-AUTH-DETAILS IO-area, and CHILD-UNQUAL-SSA. If PAUT-PCB-STATUS spaces (success), sets MORE-AUTHS, increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT (likely intended for detail counters), moves segment to CHILD-SEG-REC, and writes OPFIL2-REC. If 'GE' (segment not found/end children), sets WS-END-OF-CHILD-SEG='Y' and displays flag. If other status, displays error/KEYFB and abends via 9999-ABEND. Initializes PAUT-PCB-STATUS at end for next call. No additional validations or transforms beyond move/write. Error handling abends on non-success/non-GE to protect data integrity.

### 4000-FILE-CLOSE
This paragraph closes the output files at program end. It displays 'CLOSING THE FILE', then CLOSE OPFILE1 and checks WS-OUTFL1-STATUS for spaces/'00'; if not, displays error but continues without abend. Similarly CLOSE OPFILE2 and checks WS-OUTFL2-STATUS with display on error. Consumes open file handles from initialization. Produces closed files and updated status flags. No business logic or decisions, purely housekeeping. Calls no paragraphs or programs. Error handling is non-fatal display only, allowing program to GOBACK cleanly.

### 9999-ABEND
This is the error termination paragraph invoked on any fatal condition. It displays 'IMSUNLOD ABENDING ...' (mismatch with program ID). Sets RETURN-CODE to 16. Performs GOBACK to exit abnormally. Consumes no specific inputs beyond context. Produces non-zero return code for caller. No decisions or validations, unconditional abend setup. Called by open checks, IMS call failures. No further calls.

## Open Questions

- ? Exact field layouts and all fields in copybooks CIPAUSMY, CIPAUDTY, PAUTBPCB, IMSFUNCS
  - Context: Copybooks are referenced but contents not provided in source; limits field-level data flow details
- ? Purpose of unused variables like WS-TOT-REC-WRITTEN, WS-NO-CHKP, WS-AUTH-SMRY-PROC-CNT (partially used), and lack of final counter display
  - Context: Counters incremented but not totaled/displayed/used; possible incomplete code or remnants
- ? WS-NO-SUMRY-READ incremented in both 2000 (root) and 3000 (child); likely bug intending WS-NO-DTL-READ in 3000
  - Context: Inconsistent counter usage observed at lines 225,268
- ? IMS PSB name and full PCB details
  - Context: Commented PSB-NAME 'IMSUNLOD' at 93; active PCB from PAUTBPCB linkage
