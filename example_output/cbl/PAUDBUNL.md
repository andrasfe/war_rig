# PAUDBUNL

**File**: `cbl/PAUDBUNL.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-25 18:32:07.681911

## Purpose

This program unloads data from an IMS hierarchical database by reading root segments named PAUTSUM0 (Pending Authorization Summary) using GN calls and writing them to sequential file OPFILE1. For each valid root segment with numeric PA-ACCT-ID, it reads associated child segments PAUTDTL1 (Pending Authorization Details) using GNP calls and writes them to OPFILE2 prefixed with the root PA-ACCT-ID as ROOT-SEG-KEY. It processes until end of database (GB status) and handles errors by abending.

**Business Context**: Utility for unloading Pending Authorization IMS database segments, likely for archiving, migration, or batch processing.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment containing Pending Authorization Summary data defined by PENDING-AUTH-SUMMARY |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment containing Pending Authorization Details data defined by PENDING-AUTH-DETAILS |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OPFILE1 | IOType.FILE_SEQUENTIAL | Sequential file containing unloaded root summary segments (OPFIL1-REC PIC X(100)) |
| OPFILE2 | IOType.FILE_SEQUENTIAL | Sequential file containing unloaded child detail segments prefixed with root segment key (ROOT-SEG-KEY PIC S9(11) COMP-3 + CHILD-SEG-REC PIC X(200)) |

## Business Rules

- **BR001**: Only write root summary record and process child details if PA-ACCT-ID is numeric
- **BR002**: Increment counters for every root and child segment read successfully from IMS

## Paragraphs/Procedures

### MAIN-PARA
Main control logic: initialize, loop over root segments until end, close files

### 1000-INITIALIZE
Accept dates, display startup info, open output files OPFILE1 and OPFILE2

### 2000-FIND-NEXT-AUTH-SUMMARY
Perform IMS GN call to read next root summary segment, write to OPFILE1 if valid numeric acct ID, read children

### 3000-FIND-NEXT-AUTH-DTL
Perform IMS GNP call to read next child detail segment under current root, write to OPFILE2

### 4000-FILE-CLOSE
Close output files OPFILE1 and OPFILE2

### 9999-ABEND
Display abend message and terminate with RETURN-CODE 16

## Open Questions

- ? Contents and field definitions of copybooks CIPAUSMY, CIPAUDTY, PAUTBPCB, IMSFUNCS
  - Context: Not provided in source; only referenced via COPY statements
- ? Exact field definition and position of PA-ACCT-ID
  - Context: Referenced in move and numeric check but defined in unprovided copybook CIPAUSMY
- ? Purpose of unused variables like WS-EXPIRY-DAYS, WS-NO-DTL-READ, WS-NO-SUMRY-DELETED
  - Context: Defined in WS but no usage in provided code; counters like WS-NO-SUMRY-READ incremented in child logic possibly erroneously
- ? Calling context and how PAUTBPCB is populated
  - Context: Passed via linkage and ENTRY 'DLITCBL'; no caller specified
