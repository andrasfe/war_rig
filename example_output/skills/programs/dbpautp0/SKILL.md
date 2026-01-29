---
name: dbpautp0
description: "This JCL job unloads the DBD DBPAUTP0 from an IMS database. It first deletes the output dataset if it exists, then executes the DFSRRC00 program with the ULU parameter to perform the unload. It allocates necessary datasets for IMS processing, including RESLIB, PSBLIB, DBDLIB, and RECON datasets."
---

# DBPAUTP0

**Type:** JCL (BATCH)

## Purpose

This JCL job unloads the DBD DBPAUTP0 from an IMS database. It first deletes the output dataset if it exists, then executes the DFSRRC00 program with the ULU parameter to perform the unload. It allocates necessary datasets for IMS processing, including RESLIB, PSBLIB, DBDLIB, and RECON datasets.

## Called Programs

- IEFBR14 (STATIC_CALL)
- DFSRRC00 (STATIC_CALL)

## Inputs

- **OEM.IMS.IMSP.PSBLIB** (FILE_SEQUENTIAL): IMS PSB Library
- **OEM.IMS.IMSP.DBDLIB** (FILE_SEQUENTIAL): IMS DBD Library
- **OEM.IMS.IMSP.PAUTHDB** (FILE_SEQUENTIAL): IMS PAUTHDB
- **OEM.IMS.IMSP.PAUTHDBX** (FILE_SEQUENTIAL): IMS PAUTHDBX
- **OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB)** (FILE_SEQUENTIAL): DFSVSMDB proc
- *(+5 more inputs)*

## Outputs

- **AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0** (FILE_VSAM): Output dataset containing the unloaded DBD DBPAUTP0 data.
- **SYSUDUMP** (REPORT): System dump output.
- **SYSPRINT** (REPORT): System print output.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of DBPAUTP0
- Trace program calls from DBPAUTP0
- Identify inputs/outputs for DBPAUTP0
- Maintain or modify DBPAUTP0

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.