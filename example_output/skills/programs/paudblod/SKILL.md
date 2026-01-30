---
name: paudblod
description: "PAUDBLOD is an IMS database loader utility that sequentially reads root segment records from INFILE1 and inserts them as PAUTSUM0 segments into the IMS database using unqualified ISRT calls. It then reads child segment records from INFILE2, which include a ROOT-SEG-KEY, uses a qualified GU call to position on the corresponding root segment, and inserts the child as PAUTDTL1 segments using unqualified ISRT. Duplicate segments (status 'II') are skipped without error, while other IMS errors or file I/O failures cause an ABEND with return code 16."
---

# PAUDBLOD

**Type:** COBOL (BATCH)
**Context:** Supports loading pending authorization summary and detail data into an IMS hierarchical database for authorization processing, likely part of a financial or payment system given segment names like PAUTSUM0 (Pending Auth Summary) and PAUTDTL1 (Pending Auth Details).

## Purpose

PAUDBLOD is an IMS database loader utility that sequentially reads root segment records from INFILE1 and inserts them as PAUTSUM0 segments into the IMS database using unqualified ISRT calls. It then reads child segment records from INFILE2, which include a ROOT-SEG-KEY, uses a qualified GU call to position on the corresponding root segment, and inserts the child as PAUTDTL1 segments using unqualified ISRT. Duplicate segments (status 'II') are skipped without error, while other IMS errors or file I/O failures cause an ABEND with return code 16.

## Business Rules

- **BR001**: Skip insertion of root or child segments if already exists in database (IMS status 'II' indicates duplicate).
- **BR002**: Only process child records with numeric ROOT-SEG-KEY to ensure valid qualification.

## Inputs

- **INFILE1** (FILE_SEQUENTIAL): Sequential file containing fixed-length 100-byte root segment records for PAUTSUM0 (Pending Authorization Summary) segments.
- **INFILE2** (FILE_SEQUENTIAL): Sequential file containing child segment records for PAUTDTL1 (Pending Authorization Details), prefixed with 11-digit COMP-3 ROOT-SEG-KEY matching root ACCNTID.
- **PAUTBPCB** (IMS_SEGMENT): IMS PCB mask providing database status, segment name, key feedback, and positioning for PAUT database access.

## Outputs

- **PAUTSUM0** (IMS_SEGMENT): Root segments (Pending Authorization Summary) inserted into IMS database.
- **PAUTDTL1** (IMS_SEGMENT): Child segments (Pending Authorization Details) inserted under matching PAUTSUM0 root segments.

## Copybooks Used

- **IMSFUNCS**: Defines IMS DL/I function codes such as FUNC-ISRT and FUNC-GU used in CBLTDLI calls.
- **CIPAUSMY**: Defines layout of PENDING-AUTH-SUMMARY root segment (PAUTSUM0) for IMS database.
- **CIPAUDTY**: Defines layout of PENDING-AUTH-DETAILS child segment (PAUTDTL1) for IMS database.
- **PAUTBPCB**: Defines PCB mask structure for PAUT IMS database including status, segment name, and key feedback fields.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PAUDBLOD
- Understand business rules implemented in PAUDBLOD
- Identify inputs/outputs for PAUDBLOD
- Maintain or modify PAUDBLOD

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.