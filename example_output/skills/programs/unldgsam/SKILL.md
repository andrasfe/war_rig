---
name: unldgsam
description: "This JCL job executes the IMS program DFSRRC00 to unload a GSAM database. It specifies the program parameters, required libraries, and input/output datasets for the IMS database unload process."
---

# UNLDGSAM

**Type:** JCL (BATCH)

## Purpose

This JCL job executes the IMS program DFSRRC00 to unload a GSAM database. It specifies the program parameters, required libraries, and input/output datasets for the IMS database unload process.

## Called Programs

- DFSRRC00 (STATIC_CALL)

## Inputs

- **AWS.M2.CARDDEMO.PAUTDB.ROOT.GSAM** (FILE_SEQUENTIAL): Input GSAM database root segment to be unloaded.
- **AWS.M2.CARDDEMO.PAUTDB.CHILD.GSAM** (FILE_SEQUENTIAL): Input GSAM database child segment to be unloaded.
- **OEM.IMS.IMSP.PAUTHDB** (FILE_SEQUENTIAL): Input IMS PAUTHDB dataset.
- **OEM.IMS.IMSP.PAUTHDBX** (FILE_SEQUENTIAL): Input IMS PAUTHDBX dataset.
- **OEMA.IMS.IMSP.SDFSRESL** (FILE_SEQUENTIAL): IMS RESLIB
- *(+5 more inputs)*

## Outputs

- **SYSPRINT** (REPORT): System print output for the job.
- **SYSUDUMP** (REPORT): System dump output for the job.
- **IMSERR** (REPORT): IMS error output for the job.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of UNLDGSAM
- Trace program calls from UNLDGSAM
- Identify inputs/outputs for UNLDGSAM
- Maintain or modify UNLDGSAM

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.