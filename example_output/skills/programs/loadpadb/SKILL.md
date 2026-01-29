---
name: loadpadb
description: "This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and input files required for the database load process."
---

# LOADPADB

**Type:** JCL (BATCH)
**Context:** Database maintenance and loading.

## Purpose

This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and input files required for the database load process.

## Called Programs

- DFSRRC00 (STATIC_CALL)

## Inputs

- **OEMA.IMS.IMSP.SDFSRESL** (FILE_SEQUENTIAL): IMS RESLIB library.
- **AWS.M2.CARDDEMO.LOADLIB** (FILE_SEQUENTIAL): Application load library.
- **OEM.IMS.IMSP.PSBLIB** (FILE_SEQUENTIAL): IMS PSB library.
- **OEM.IMS.IMSP.DBDLIB** (FILE_SEQUENTIAL): IMS DBD library.
- **AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO** (FILE_SEQUENTIAL): Input file for the PAUTDB root segment.
- *(+2 more inputs)*

## Outputs

- **SYSPRINT** (REPORT): System print output.
- **SYSUDUMP** (REPORT): System dump output.
- **IMSERR** (REPORT): IMS error output.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of LOADPADB
- Trace program calls from LOADPADB
- Identify inputs/outputs for LOADPADB
- Maintain or modify LOADPADB

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.