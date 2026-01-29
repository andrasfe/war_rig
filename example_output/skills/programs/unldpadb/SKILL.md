---
name: unldpadb
description: This JCL unloads the PAUTDB IMS database to sequential files. It executes the IMS program DFSRRC00 with the PARM parameter specifying the DLI function and the PAUDBUNL application program. It also deletes and recreates the output files.
---

# UNLDPADB

**Type:** JCL (BATCH)
**Context:** UNKNOWN

## Purpose

This JCL unloads the PAUTDB IMS database to sequential files. It executes the IMS program DFSRRC00 with the PARM parameter specifying the DLI function and the PAUDBUNL application program. It also deletes and recreates the output files.

## Called Programs

- IEFBR14 (STATIC_CALL)
- DFSRRC00 (STATIC_CALL)

## Inputs

- **OEM.IMS.IMSP.SDFSRESL** (FILE_SEQUENTIAL): IMS RESLIB
- **OEM.IMS.IMSP.PSBLIB** (FILE_SEQUENTIAL): IMS PSBLIB
- **OEM.IMS.IMSP.DBDLIB** (FILE_SEQUENTIAL): IMS DBDLIB
- **OEM.IMS.IMSP.PAUTHDB** (FILE_SEQUENTIAL): PAUTHDB IMS database
- **OEM.IMS.IMSP.PAUTHDBX** (FILE_SEQUENTIAL): PAUTHDBX IMS database
- *(+1 more inputs)*

## Outputs

- **AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO** (FILE_SEQUENTIAL): Unloaded PAUTDB root segment data
- **AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO** (FILE_SEQUENTIAL): Unloaded PAUTDB child segment data

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of UNLDPADB
- Trace program calls from UNLDPADB
- Identify inputs/outputs for UNLDPADB
- Maintain or modify UNLDPADB

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.