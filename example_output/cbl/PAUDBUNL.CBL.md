# PAUDBUNL

**File**: `cbl/PAUDBUNL.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-27 02:42:45.463999

## Purpose

PAUDBUNL is an IMS database unload utility that retrieves all PENDING-AUTH-SUMMARY root segments (PAUTSUM0) using unqualified GN calls and writes them directly to sequential file OPFILE1. For each root segment, it then retrieves dependent PENDING-AUTH-DETAILS child segments (PAUTDTL1) using unqualified GNP calls and writes them to OPFILE2 prefixed with the root segment's PA-ACCT-ID as ROOT-SEG-KEY. The process continues until the end of the database (status 'GB') is reached, with counters tracking records read and written.

**Business Context**: Unloads pending authorization summary and detail data from IMS PAUT database for backup, migration, or offline processing.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | PENDING-AUTH-SUMMARY root segments from IMS PAUT database |
| PAUTDTL1 | IOType.IMS_SEGMENT | PENDING-AUTH-DETAILS child segments dependent on PAUTSUM0 from IMS PAUT database |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OPFILE1 | IOType.FILE_SEQUENTIAL | Unloaded PENDING-AUTH-SUMMARY root segments copied directly from IMS |
| OPFILE2 | IOType.FILE_SEQUENTIAL | Unloaded PENDING-AUTH-DETAILS child segments prefixed with root PA-ACCT-ID as ROOT-SEG-KEY |

## Business Rules

- **BR001**: Only process root segment if PA-ACCT-ID is numeric before writing to OPFILE1 and proceeding to children
- **BR002**: Abort program on any IMS PCB status other than spaces (success) or 'GB' (end of DB) during GN call
- **BR003**: Abort program on any IMS PCB status other than spaces (success) or 'GE' (segment not found) during GNP call

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point paragraph that orchestrates the entire unload process and supports an alternate entry point 'DLITCBL' for potential dynamic invocation. It consumes no direct inputs but relies on the LINKAGE SECTION PAUTBPCB for IMS database access via the calling environment. It first performs 1000-INITIALIZE to open output files, initialize dates, and display startup messages. It then enters a loop performing 2000-FIND-NEXT-AUTH-SUMMARY until WS-END-OF-ROOT-SEG is set to 'Y', which handles reading and writing all root segments and their children. After the loop completes upon reaching end of database, it performs 4000-FILE-CLOSE to shut down files gracefully. No business decisions or validations are made here; it purely controls flow and delegates processing. Error handling is delegated to subordinate paragraphs which abend on failures. It produces no direct outputs but enables all file writes through performed paragraphs. Counters like WS-NO-SUMRY-READ are indirectly updated via calls.

### 1000-INITIALIZE
This initialization paragraph sets up the program environment by accepting system dates into CURRENT-DATE and CURRENT-YYDDD, displaying startup information including program name and date. It opens output files OPFILE1 and OPFILE2 in OUTPUT mode, checking WS-OUTFL1-STATUS and WS-OUTFL2-STATUS after each OPEN. If file status is not spaces or '00', it displays the error and performs 9999-ABEND to terminate. It consumes no data inputs beyond system date but initializes WS variables implicitly. It produces open files ready for writes and display output to console. No business logic or IMS calls here; purely setup and validation of output files. Error handling is local: immediate abend on open failure. No paragraphs or programs called.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph handles retrieval and processing of each PENDING-AUTH-SUMMARY root segment using an IMS GN call with unqualified SSA ROOT-UNQUAL-SSA. It initializes PAUT-PCB-STATUS before the CBLTDLI call using FUNC-GN, PAUTBPCB, PENDING-AUTH-SUMMARY IO area, and SSA. On success (status spaces), it increments counters WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves the segment to OPFIL1-REC, sets ROOT-SEG-KEY from PA-ACCT-ID, and writes to OPFILE1 if PA-ACCT-ID is numeric. It then initializes child flags and performs 3000-FIND-NEXT-AUTH-DTL in a loop until WS-END-OF-CHILD-SEG='Y' to unload dependents. On 'GB' status, sets end flags for root loop exit. On other statuses, displays error details including PAUT-KEYFB and abends via 9999-ABEND. Inputs are IMS database via PCB; outputs are writes to OPFILE1 and triggers child processing. Business logic enforces numeric PA-ACCT-ID before write and child loop.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph retrieves the next dependent PENDING-AUTH-DETAILS child segment for the current root using IMS GNP call with unqualified SSA CHILD-UNQUAL-SSA. It calls CBLTDLI using FUNC-GNP, PAUTBPCB, PENDING-AUTH-DETAILS IO area, and SSA. On success (spaces), increments WS-NO-DTL-READ (though counter add commented or mismatched), moves segment to CHILD-SEG-REC, and writes OPFIL2-REC. On 'GE' (no more segments), sets WS-END-OF-CHILD-SEG to 'Y' and displays flag. On other statuses, displays PAUT-PCB-STATUS and PAUT-KEYFB, then abends via 9999-ABEND. It initializes PAUT-PCB-STATUS at end for next call. Inputs from IMS child segments under current root; outputs writes to OPFILE2. Business logic: continues until no more children per root. Error handling: abend on unexpected IMS statuses.

### 4000-FILE-CLOSE
This termination paragraph closes output files OPFILE1 and OPFILE2, displaying 'CLOSING THE FILE' message. It checks WS-OUTFL1-STATUS and WS-OUTFL2-STATUS after each CLOSE; if not spaces or '00', displays error but does not abend (unlike open). Inputs are the opened files; outputs are closed files and potential console messages. No business logic, IMS calls, or validations beyond status check. Error handling: displays close errors but continues. No subordinate calls.

### 9999-ABEND
This error termination paragraph displays 'IMSUNLOD ABENDING ...' (mismatched program name comment), sets RETURN-CODE to 16, and GOBACKs. It consumes error context from caller (e.g., status codes displayed prior). Produces abend with RC=16 and console message. No inputs read, no business logic, no validations. Handles all program errors by uniform abend. Called only on failures from other paragraphs.

## Open Questions

- ? Exact field definitions in copybooks CIPAUSMY and CIPAUDTY
  - Context: Copybooks are referenced but contents not provided in source
- ? Purpose of counters like WS-NO-CHKP, WS-NO-DTL-DELETED (initialized but not used)
  - Context: Defined and zeroed but no increments or displays in provided code
- ? IMS PSB name and full PCB details
  - Context: PSB-NAME commented out; relies on PAUTBPCB linkage
