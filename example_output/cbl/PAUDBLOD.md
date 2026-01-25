# PAUDBLOD

**File**: `cbl/PAUDBLOD.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-25 18:31:30.680436

## Purpose

Batch program that loads root segments (PAUTSUM0 pending authorization summaries) from INFILE1 and child segments (PAUTDTL1 pending authorization details) from INFILE2 into an IMS database. Inserts root segments via unconditional ISRT (ignoring duplicates), then for each child segment performs qualified GU on the matching root segment followed by child ISRT (ignoring duplicates). Abends on IMS call failures other than success or duplicate.

**Business Context**: Loads pending authorization summary and detail data into IMS database for authorization processing

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | IOType.FILE_SEQUENTIAL | Sequential file containing root segment records mapped to PENDING-AUTH-SUMMARY (PAUTSUM0) |
| INFILE2 | IOType.FILE_SEQUENTIAL | Sequential file containing root segment keys (ROOT-SEG-KEY) and child segment records (CHILD-SEG-REC) mapped to PENDING-AUTH-DETAILS (PAUTDTL1) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | IMS root segments (pending authorization summaries) inserted via ISRT |
| PAUTDTL1 | IOType.IMS_SEGMENT | IMS child segments (pending authorization details) inserted via ISRT under qualified root |

## Business Rules

- **BR001**: Insert root segments unconditionally, ignoring duplicates
- **BR002**: For each child segment, first GU the parent root segment using ROOT-SEG-KEY qualifier
- **BR003**: Insert child segments under qualified root, ignoring duplicates
- **BR004**: Skip non-numeric ROOT-SEG-KEY in child records

## Paragraphs/Procedures

### MAIN-PARA
Main control logic: initializes, loops to load all root segments from INFILE1, then loops to load all child segments from INFILE2, closes files

### 1000-INITIALIZE
Accepts date, displays it, opens input files INFILE1 and INFILE2 with status checks

### 2000-READ-ROOT-SEG-FILE
Reads next record from INFILE1, moves to PENDING-AUTH-SUMMARY if successful, performs root insert; sets EOF flag on status 10

### 2100-INSERT-ROOT-SEG
Calls CBLTDLI ISRT to insert root segment with ROOT-UNQUAL-SSA; handles status (success or II ok, else abend)

### 3000-READ-CHILD-SEG-FILE
Reads next record from INFILE2; if ROOT-SEG-KEY numeric and read ok, moves data and performs child insert; sets EOF on status 10

### 3100-INSERT-CHILD-SEG
Performs GU on root using ROOT-QUAL-SSA (fails if not SPACES or II); if ok performs child insert

### 3200-INSERT-IMS-CALL
Calls CBLTDLI ISRT to insert child segment with CHILD-UNQUAL-SSA; handles status (success or II ok, else abend)

### 4000-FILE-CLOSE
Closes INFILE1 and INFILE2 with status checks (displays errors but no abend)

### 9999-ABEND
Displays abend message, sets return-code 16, gobacks

## Open Questions

- ? Exact field layouts and keys in CIPAUSMY (PAUTSUM0) and CIPAUDTY (PAUTDTL1) copybooks
  - Context: Copybooks referenced but source not provided; inferred from usage (e.g. ACCNTID key in SSA line 116)
- ? Purpose and source of PRM-INFO working storage fields (P-EXPIRY-DAYS, P-CHKP-FREQ, etc.)
  - Context: Defined lines 129-139 but never populated or referenced in code
- ? Usage of counters like WS-TOT-REC-WRITTEN, WS-NO-DTL-READ (lines 65-69)
  - Context: Defined and initialized to 0 but never incremented or displayed
