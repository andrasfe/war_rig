# DBUNLDGS

**File**: `cbl/DBUNLDGS.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-25 18:31:55.663789

## Purpose

DBUNLDGS is an IMS batch utility program that reads all root pending authorization summary segments (PAUTSUM0) from the PAUT IMS database using GN calls and their dependent child detail segments (PAUTDTL1) using GNP calls. It inserts the summary segments into the PASFL GSAM database and detail segments into the PADFL GSAM database via ISRT calls. Sequential file outputs are defined but commented out.

**Business Context**: Unloading pending authorization data from hierarchical IMS database PAUT to GSAM databases PASFL/PADFL for processing or archiving

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.IMS_SEGMENT | IMS application PCB providing access to PAUT database root summary (PAUTSUM0) and child detail (PAUTDTL1) segments |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFLPCB | IOType.IMS_SEGMENT | GSAM PCB receiving inserted pending authorization summary segments from PAUT |
| PADFLPCB | IOType.IMS_SEGMENT | GSAM PCB receiving inserted pending authorization detail segments from PAUT |

## Business Rules

- **BR001**: Only insert summary segment and process its child details if the account ID is numeric
- **BR002**: Terminate root segment loop on end-of-database condition
- **BR003**: Terminate child segment loop for current parent on no-more-segments condition

## Paragraphs/Procedures

### MAIN-PARA
Program entry point: performs initialization, loops over root summaries processing children until end, then closes

### 1000-INITIALIZE
Accepts and displays current date, startup messages (files open commented out)

### 2000-FIND-NEXT-AUTH-SUMMARY
Performs GN call to read next root summary, inserts to GSAM if valid and account numeric, loops children

### 3000-FIND-NEXT-AUTH-DTL
Performs GNP call to read next child detail under current root, inserts to GSAM if found

### 3100-INSERT-PARENT-SEG-GSAM
Performs ISRT to insert summary segment into PASFL GSAM

### 3200-INSERT-CHILD-SEG-GSAM
Performs ISRT to insert detail segment into PADFL GSAM

### 4000-FILE-CLOSE
Closes output files (logic commented out)

### 9999-ABEND
Displays abend message and sets return code 16

## Open Questions

- ? Exact field layouts and key fields in copybooks CIPAUSMY and CIPAUDTY
  - Context: Copybooks referenced but source not provided; field names like PA-ACCT-ID inferred from usage
- ? Business purpose of unloading to GSAM PASFL/PADFL
  - Context: Inferred as data migration/archiving but no explicit documentation in code
- ? Usage of counters like WS-NO-SUMRY-READ (incremented for children?) and why no final display
  - Context: Counters updated but not output; possible code error in child counter name
