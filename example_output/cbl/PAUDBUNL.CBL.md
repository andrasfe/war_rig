# PAUDBUNL

**File**: `cbl/PAUDBUNL.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 17:37:56.675601

## Purpose

This IMS batch unload program reads root segments (PAUTSUM0 Pending Authorization Summary) from the IMS database using GN calls and writes them to sequential file OPFILE1. For each root segment, it then reads child segments (PAUTDTL1 Pending Authorization Details) using GNP calls and writes them to OPFILE2, prefixed with the root account ID as ROOT-SEG-KEY. It continues until the end of the database (GB status) and handles errors by abending.

**Business Context**: Supports unloading of pending authorization data from IMS database for archiving, migration, or offline processing.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Pending Authorization Summary root segment from IMS database |
| PAUTDTL1 | IOType.IMS_SEGMENT | Pending Authorization Details child segment from IMS database, qualified under root |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OPFILE1 | IOType.FILE_SEQUENTIAL | Contains copied root segments (Pending Authorization Summary records) |
| OPFILE2 | IOType.FILE_SEQUENTIAL | Contains root segment key (PA-ACCT-ID) followed by child segments (Pending Authorization Details records) |

## Business Rules

- **BR001**: Only write root record and process children if PA-ACCT-ID is numeric

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph that controls the overall program flow. It begins with an ENTRY statement 'DLITCBL' using the PAUTBPCB linkage for IMS access. It first performs 1000-INITIALIZE to set up dates, displays, and open output files OPFILE1 and OPFILE2. After initialization, it enters a loop performing 2000-FIND-NEXT-AUTH-SUMMARY until WS-END-OF-ROOT-SEG is 'Y', processing each root segment and its children. Once the end of the database is reached, it performs 4000-FILE-CLOSE to shut down files. No explicit error handling here beyond subordinate paragraphs; it relies on abend in errors. It then GOBACKs with the final return code. No parameters consumed directly; relies on IMS PCB passed via linkage.

### 1000-INITIALIZE
This paragraph handles program initialization including date acceptance and output file opens. It consumes system date via ACCEPT CURRENT-DATE FROM DATE and CURRENT-YYDDD FROM DAY. It displays startup messages and current date for logging. It then opens OPFILE1 and checks WS-OUTFL1-STATUS; if not spaces or '00', displays error and performs 9999-ABEND. Similarly opens OPFILE2 and checks WS-OUTFL2-STATUS with same error handling. Produces no data outputs but prepares files for writing. No business decisions beyond status validation. Error handling is immediate abend on open failure. No calls to other paragraphs.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph performs the root segment read loop using IMS GN call and orchestrates child processing. It initializes PAUT-PCB-STATUS, calls CBLTDLI with FUNC-GN, PAUTBPCB, PENDING-AUTH-SUMMARY IO area, and ROOT-UNQUAL-SSA. Consumes IMS root segments (PAUTSUM0). If PAUT-PCB-STATUS is spaces (success), increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT counters, moves segment to OPFIL1-REC. Sets ROOT-SEG-KEY from PA-ACCT-ID, and if PA-ACCT-ID numeric, writes to OPFILE1 and performs 3000-FIND-NEXT-AUTH-DTL loop until WS-END-OF-CHILD-SEG='Y'. If status 'GB', sets end flags for root and database. If other status, displays error and key feedback, then abends via 9999-ABEND. Business logic includes numeric check on account ID to avoid invalid writes. Produces root records in OPFILE1 and triggers child writes.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph reads child segments under the current root using IMS GNP call. It calls CBLTDLI with FUNC-GNP, PAUTBPCB, PENDING-AUTH-DETAILS IO area, and CHILD-UNQUAL-SSA. Consumes IMS child segments (PAUTDTL1). If PAUT-PCB-STATUS spaces (success), sets MORE-AUTHS flag, increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT (note: counters named for summary but used here), moves segment to CHILD-SEG-REC, and writes OPFIL2-REC (with pre-set ROOT-SEG-KEY). If 'GE' (segment not found), sets WS-END-OF-CHILD-SEG to 'Y' and displays flag. If other status, displays error and key feedback, then abends. Initializes PAUT-PCB-STATUS at end. No additional decisions; relies on GNP for parentage. Produces child records in OPFILE2 prefixed by root key. Error handling abends on unexpected IMS status.

### 4000-FILE-CLOSE
This paragraph closes output files at program termination. It displays 'CLOSING THE FILE', then CLOSE OPFILE1 and checks WS-OUTFL1-STATUS; if not spaces or '00', displays error but does not abend. Similarly CLOSE OPFILE2 and checks WS-OUTFL2-STATUS with display on error. Consumes no inputs beyond file handles. Produces no data but ensures clean file closure. No business logic or decisions. Error handling limited to display without abend. No calls to other paragraphs.

### 9999-ABEND
This is the error termination paragraph invoked on any fatal error. It displays 'IMSUNLOD ABENDING ...' (note: program is PAUDBUNL but message references IMSUNLOD). Sets RETURN-CODE to 16. Then GOBACKs to caller. Consumes no specific data; triggered by PERFORM from error sites. Produces abend with RC 16. No conditions checked here. No validation or business logic. No subordinate calls.

## Open Questions

- ? Exact contents and field layouts of copybooks CIPAUSMY, CIPAUDTY, PAUTBPCB, IMSFUNCS
  - Context: Copybooks define segment layouts and PCB but source not provided
- ? Purpose of unused variables like WS-NO-CHKP, WS-EXPIRY-DAYS, WS-DAY-DIFF, PRM-INFO
  - Context: Defined but never referenced in code
- ? Why WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT incremented in child paragraph 3000
  - Context: Counters named for summary but incremented on child success (lines 268-269)
- ? IMS database name and full PSB details
  - Context: PSB name commented as IMSUNLOD; PCB is PAUTBPCB
