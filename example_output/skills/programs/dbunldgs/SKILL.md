---
name: dbunldgs
description: "DBUNLDGS is an IMS batch utility that browses and unloads root Pending Authorization Summary segments (PAUTSUM0) and their child Pending Authorization Detail segments (PAUTDTL1) from the PAUT IMS database using unqualified SSAs via the PAUTBPCB. For each root segment retrieved via GN call, it inserts a copy into a GSAM database using PASFLPCB and then retrieves all children via GNP calls, inserting each into another GSAM via PADFLPCB. It continues until end of database (GB status), increments counters, and abends on errors."
---

# DBUNLDGS

**Type:** COBOL (BATCH)
**Context:** Supports unloading of pending authorization data from IMS for potential archiving, migration, or reporting in a financial authorization system.

## Purpose

DBUNLDGS is an IMS batch utility that browses and unloads root Pending Authorization Summary segments (PAUTSUM0) and their child Pending Authorization Detail segments (PAUTDTL1) from the PAUT IMS database using unqualified SSAs via the PAUTBPCB. For each root segment retrieved via GN call, it inserts a copy into a GSAM database using PASFLPCB and then retrieves all children via GNP calls, inserting each into another GSAM via PADFLPCB. It continues until end of database (GB status), increments counters, and abends on errors.

## Business Rules

- **BR001**: Process only valid root segments with numeric PA-ACCT-ID before handling children
- **BR002**: End root processing on GB status from PAUTBPCB
- **BR003**: End child processing on GE status from PAUTBPCB

## Inputs

- **PAUT IMS Database** (IMS_SEGMENT): Root PAUTSUM0 summary segments and child PAUTDTL1 detail segments accessed via PAUTBPCB browse PCB

## Outputs

- **PASFL GSAM** (IMS_SEGMENT): Copies of unloaded PAUTSUM0 root summary segments inserted via PASFLPCB
- **PADFL GSAM** (IMS_SEGMENT): Copies of unloaded PAUTDTL1 child detail segments inserted via PADFLPCB

## Copybooks Used

- **IMSFUNCS**: Defines IMS DL/I function codes like FUNC-GN, FUNC-GNP, FUNC-ISRT
- **CIPAUSMY**: Defines layout of PENDING-AUTH-SUMMARY root segment including PA-ACCT-ID
- **CIPAUDTY**: Defines layout of PENDING-AUTH-DETAILS child segment
- **PAUTBPCB**: PCB mask for browsing PAUT IMS database (PAUTSUM0/PAUTDTL1)
- **PASFLPCB**: PCB mask for inserting into summary GSAM database
- **PADFLPCB**: PCB mask for inserting into details GSAM database

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of DBUNLDGS
- Understand business rules implemented in DBUNLDGS
- Identify inputs/outputs for DBUNLDGS
- Maintain or modify DBUNLDGS

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.