---
name: loadpadb
description: This JCL job executes the IMS utility program DFSRRC00 in BMP mode to load the PAUTDB IMS database using PSBPAUTB and database reference PAUDBLOD. It reads root and child segment data from sequential input files INFILE1 and INFILE2. Standard IMS libraries and control datasets are referenced for execution.
---

# LOADPADB

**Type:** JCL (BATCH)
**Context:** Loads data into the PAUTDB IMS database as part of the AWS M2 Card Demo application

## Purpose

This JCL job executes the IMS utility program DFSRRC00 in BMP mode to load the PAUTDB IMS database using PSBPAUTB and database reference PAUDBLOD. It reads root and child segment data from sequential input files INFILE1 and INFILE2. Standard IMS libraries and control datasets are referenced for execution.

## Called Programs

- DFSRRC00 (STATIC_CALL)

## Inputs

- **INFILE1** (FILE_SEQUENTIAL): Root segment unload file for PAUTDB database load
- **INFILE2** (FILE_SEQUENTIAL): Child segment unload file for PAUTDB database load
- **IMS** (OTHER): IMS PSBLIB and DBDLIB for PSB and DBD definitions
- **STEPLIB** (OTHER): Load libraries including IMS SDFSRESL and application LOADLIB

## Outputs

- **SYSPRINT** (REPORT): Standard print output for job logs
- **SYSUDUMP** (REPORT): System dump output for abends
- **IMSERR** (REPORT): IMS error output
- **PAUTDB** (IMS_SEGMENT): Target IMS database loaded with root and child segments (inferred from PARM and inputs)

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of LOADPADB
- Trace program calls from LOADPADB
- Identify inputs/outputs for LOADPADB
- Maintain or modify LOADPADB

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.