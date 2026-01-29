---
name: dbunldgs
description: "This COBOL program, named DBUNLDGS, extracts data related to pending authorizations from an IMS database. It retrieves pending authorization summary and detail segments and writes them to GSAM files. The program uses IMS calls to navigate the database and GSAM calls to write the extracted data."
---

# DBUNLDGS

**Type:** COBOL (BATCH)

## Purpose

This COBOL program, named DBUNLDGS, extracts data related to pending authorizations from an IMS database. It retrieves pending authorization summary and detail segments and writes them to GSAM files. The program uses IMS calls to navigate the database and GSAM calls to write the extracted data.

## Business Rules

- **BR001**: The program processes pending authorization summary records only if the account ID (PA-ACCT-ID) is numeric.
- **BR002**: The program retrieves pending authorization detail records until the end of the child segment is reached.

## Called Programs

- CBLTDLI (STATIC_CALL)
- CBLTDLI (STATIC_CALL)
- CBLTDLI (STATIC_CALL)
- CBLTDLI (STATIC_CALL)

## Inputs

- **PAUTBPCB** (PARAMETER): Pending Authorization Summary PCB mask
- **PASFLPCB** (PARAMETER): Pending Authorization Summary GSAM PCB mask
- **PADFLPCB** (PARAMETER): Pending Authorization Detail GSAM PCB mask

## Outputs

- **OPFILE1** (FILE_SEQUENTIAL): Output file containing pending authorization summary records.
- **OPFILE2** (FILE_SEQUENTIAL): Output file containing pending authorization detail records.

## Copybooks Used

- **IMSFUNCS**: Contains definitions for IMS function codes used in DL/I calls.
- **CIPAUSMY**: Defines the layout of the PENDING-AUTH-SUMMARY segment.
- **CIPAUDTY**: Defines the layout of the PENDING-AUTH-DETAILS segment.
- **PAUTBPCB**: Defines the PCB mask for the Pending Authorization database.
- **PASFLPCB**: Defines the PCB mask for the GSAM output file for summary records.
- **PADFLPCB**: Defines the PCB mask for the GSAM output file for detail records.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of DBUNLDGS
- Understand business rules implemented in DBUNLDGS
- Trace program calls from DBUNLDGS
- Identify inputs/outputs for DBUNLDGS
- Maintain or modify DBUNLDGS

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.