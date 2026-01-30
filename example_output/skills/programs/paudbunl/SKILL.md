---
name: paudbunl
description: "PAUDBUNL unloads authorization summary records from an IMS database by performing DL/I GN calls on root segments, writing each summary to OPFILE1 after extracting account ID, and then processing child detail segments via a subordinate paragraph until no more children. It initializes by opening output files OPFILE1 and OPFILE2 with status checks, loops until end of root segments (IMS status 'GB'), and closes files at termination. The program displays startup information including current date."
---

# PAUDBUNL

**Type:** COBOL (BATCH)
**Context:** Unloading IMS authorization database (summary and detail records) to sequential output files for reporting or archiving

## Purpose

PAUDBUNL unloads authorization summary records from an IMS database by performing DL/I GN calls on root segments, writing each summary to OPFILE1 after extracting account ID, and then processing child detail segments via a subordinate paragraph until no more children. It initializes by opening output files OPFILE1 and OPFILE2 with status checks, loops until end of root segments (IMS status 'GB'), and closes files at termination. The program displays startup information including current date.

## Business Rules

- **BR001**: Validate OPFILE1 open status before proceeding
- **BR002**: Validate OPFILE2 open status before proceeding
- **BR003**: Process IMS root segment only on successful read (status spaces)
- **BR004**: Detect end of IMS root segments
- **BR005**: Abort on IMS read error (neither spaces nor 'GB')

## Called Programs

- CBLTDLI (DYNAMIC_CALL)
- 9999-ABEND (STATIC_CALL)

## Inputs

- **PAUTBPCB** (IMS_SEGMENT): IMS PCB used for DL/I calls to read authorization summary root segments and child details from AUTHDB database
- **ROOT-UNQUAL-SSA** (IMS_SEGMENT): Unqualified SSA for IMS root segment access

## Outputs

- **OPFILE1** (FILE_SEQUENTIAL): Sequential output file containing authorization summary records (moved from PENDING-AUTH-SUMMARY) and potentially child details
- **OPFILE2** (FILE_SEQUENTIAL): Sequential output file opened but not written in provided code snippet; purpose unclear

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PAUDBUNL
- Understand business rules implemented in PAUDBUNL
- Trace program calls from PAUDBUNL
- Identify inputs/outputs for PAUDBUNL
- Maintain or modify PAUDBUNL

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.