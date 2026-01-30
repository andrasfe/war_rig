---
name: unldgsam
description: "This JCL submits a batch job to execute the IMS database reorganization utility DFSRRC00 in unload mode for the GSAM database DBUNLDGS using DLI access method and PSB DLIGSAMP. It provides access to required IMS libraries, PSB/DBD libraries, PAUTDB GSAM datasets (root and child), and directs utility output to SYSOUT datasets. The job is part of the AWS M2 Card Demo application dataset maintenance."
---

# UNLDGSAM

**Type:** JCL (BATCH)
**Context:** Unloading IMS PAUTDB GSAM databases (root and child) for the AWS M2 Card Demo application, likely for backup, reorg preparation, or data extraction.

## Purpose

This JCL submits a batch job to execute the IMS database reorganization utility DFSRRC00 in unload mode for the GSAM database DBUNLDGS using DLI access method and PSB DLIGSAMP. It provides access to required IMS libraries, PSB/DBD libraries, PAUTDB GSAM datasets (root and child), and directs utility output to SYSOUT datasets. The job is part of the AWS M2 Card Demo application dataset maintenance.

## Business Rules

- **BR001**: Executes IMS database unload using DLI access method on target database DBUNLDGS with PSB DLIGSAMP and specific utility flags

## Called Programs

- DFSRRC00 (STATIC_CALL)

## Inputs

- **PARM** (PARAMETER): IMS utility control parameters specifying DLI access mode, database name DBUNLDGS, PSB name DLIGSAMP, and additional flags
- **PASFILOP** (IMS_SEGMENT): Input GSAM dataset for PAUTDB.ROOT containing IMS database segments to unload
- **PADFILOP** (IMS_SEGMENT): Input GSAM dataset for PAUTDB.CHILD containing IMS database segments to unload
- **IMS** (OTHER): IMS PSBLIB and DBDLIB for program specification block and database description library
- **DDPAUTP0** (OTHER): IMS PAUTHDB library reference
- *(+2 more inputs)*

## Outputs

- **SYSPRINT** (REPORT): Utility execution reports and listings
- **SYSUDUMP** (REPORT): System dump output on abends
- **IMSERR** (REPORT): IMS-specific error messages

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of UNLDGSAM
- Understand business rules implemented in UNLDGSAM
- Trace program calls from UNLDGSAM
- Identify inputs/outputs for UNLDGSAM
- Maintain or modify UNLDGSAM

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.