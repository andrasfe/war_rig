---
name: paudbunl
description: "The COBOL program PAUDBUNL unloads pending authorization summary and detail segments from an IMS database to two sequential output files. It reads PAUTSUM0 root segments and PAUTDTL1 child segments from the IMS database and writes them to OPFILE1 and OPFILE2 respectively, provided the PA-ACCT-ID is numeric."
---

# PAUDBUNL

**Type:** COBOL (BATCH)
**Context:** This program is likely used for data extraction or migration of pending authorization data from an IMS system.

## Purpose

The COBOL program PAUDBUNL unloads pending authorization summary and detail segments from an IMS database to two sequential output files. It reads PAUTSUM0 root segments and PAUTDTL1 child segments from the IMS database and writes them to OPFILE1 and OPFILE2 respectively, provided the PA-ACCT-ID is numeric.

## Business Rules

- **BR001**: Only write summary and detail records to output files if the PA-ACCT-ID is numeric.

## Called Programs

- CBLTDLI (STATIC_CALL)
- CBLTDLI (STATIC_CALL)

## Inputs

- **IMS Database (PAUTSUM0, PAUTDTL1 Segments)** (IMS_SEGMENT): Pending authorization summary (PAUTSUM0) and detail (PAUTDTL1) segments from an IMS database.
- **PAUTBPCB** (PARAMETER): PCB mask for IMS calls.

## Outputs

- **OPFILE1** (FILE_SEQUENTIAL): Sequential file containing pending authorization summary records (PAUTSUM0) extracted from the IMS database.  Each record is 100 bytes long.
- **OPFILE2** (FILE_SEQUENTIAL): Sequential file containing pending authorization detail records (PAUTDTL1) extracted from the IMS database. Each record contains a ROOT-SEG-KEY (PA-ACCT-ID) and the CHILD-SEG-REC.

## Copybooks Used

- **IMSFUNCS**: Contains definitions for IMS function codes (e.g., FUNC-GN, FUNC-GNP).
- **CIPAUSMY**: Defines the layout of the PENDING-AUTH-SUMMARY segment (root segment).
- **CIPAUDTY**: Defines the layout of the PENDING-AUTH-DETAILS segment (child segment).
- **PAUTBPCB**: Defines the PCB mask used for IMS calls.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PAUDBUNL
- Understand business rules implemented in PAUDBUNL
- Trace program calls from PAUDBUNL
- Identify inputs/outputs for PAUDBUNL
- Maintain or modify PAUDBUNL

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.