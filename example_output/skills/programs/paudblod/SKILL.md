---
name: paudblod
description: "The COBOL program PAUDBLOD reads two sequential files, INFILE1 and INFILE2, and inserts data from these files into an IMS database. INFILE1 contains root segment data for 'PAUTSUM0', and INFILE2 contains child segment data for 'PAUTDTL1', related to pending authorizations. The program uses CBLTDLI calls to insert these segments into the IMS database."
---

# PAUDBLOD

**Type:** COBOL (BATCH)
**Context:** This program likely supports the process of loading or updating pending authorization information within an organization's IMS database, potentially for auditing or reporting purposes.

## Purpose

The COBOL program PAUDBLOD reads two sequential files, INFILE1 and INFILE2, and inserts data from these files into an IMS database. INFILE1 contains root segment data for 'PAUTSUM0', and INFILE2 contains child segment data for 'PAUTDTL1', related to pending authorizations. The program uses CBLTDLI calls to insert these segments into the IMS database.

## Called Programs

- CBLTDLI (STATIC_CALL)
- CBLTDLI (STATIC_CALL)
- CBLTDLI (STATIC_CALL)

## Inputs

- **INFILE1** (FILE_SEQUENTIAL): Contains root segment data for 'PAUTSUM0' (Pending Authorization Summary) to be inserted into the IMS database.
- **INFILE2** (FILE_SEQUENTIAL): Contains child segment data for 'PAUTDTL1' (Pending Authorization Details) to be inserted into the IMS database.
- **IO-PCB-MASK** (PARAMETER): IO PCB Mask from Linkage Section
- **PAUTBPCB** (PARAMETER): PAUTBPCB from Linkage Section

## Copybooks Used

- **IMSFUNCS**: Contains definitions for IMS function codes used in CBLTDLI calls.
- **CIPAUSMY**: Defines the layout of the PENDING-AUTH-SUMMARY segment (root segment).
- **CIPAUDTY**: Defines the layout of the PENDING-AUTH-DETAILS segment (child segment).
- **PAUTBPCB**: Defines the structure of the Program Communication Block (PCB) used for IMS communication.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PAUDBLOD
- Trace program calls from PAUDBLOD
- Identify inputs/outputs for PAUDBLOD
- Maintain or modify PAUDBLOD

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.