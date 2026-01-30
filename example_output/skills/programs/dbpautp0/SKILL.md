---
name: dbpautp0
description: "This JCL job first deletes any existing unload dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 using IEFBR14 in STEPDEL, then unloads the IMS database DBPAUTP0 using DFSRRC00 utility with PARM=(ULU,DFSURGU0,DBPAUTP0) into a new sequential VB dataset. It supports IMS database export for backup or processing in the CardDemo environment."
---

# DBPAUTP0

**Type:** JCL (BATCH)
**Context:** IMS database management for CardDemo application, unloading DBPAUTP0 (authorization database PAUTHDB) to flat file.

## Purpose

This JCL job first deletes any existing unload dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 using IEFBR14 in STEPDEL, then unloads the IMS database DBPAUTP0 using DFSRRC00 utility with PARM=(ULU,DFSURGU0,DBPAUTP0) into a new sequential VB dataset. It supports IMS database export for backup or processing in the CardDemo environment.

## Business Rules

- **BR001**: Perform database unload only under conditional activation

## Called Programs

- IEFBR14 (STATIC_CALL)
- DFSRRC00 (STATIC_CALL)

## Inputs

- **PARM** (PARAMETER): Parameters directing IMS unload: ULU (unload), DFSURGU0 (user routine), DBPAUTP0 (database DBD name)
- **DDPAUTP0** (IMS_SEGMENT): Primary IMS PAUTHDB dataset for DBPAUTP0 unload
- **DDPAUTX0** (IMS_SEGMENT): Secondary IMS PAUTHDBX dataset for DBPAUTP0 unload
- **DFSVSAMP** (OTHER): IMS sample proclib macro DFSVSMDB for database definition
- **DFSCTL** (OTHER): Control statements for IMS utility including SBPARM ACTIV=COND

## Outputs

- **DFSURGU1** (FILE_SEQUENTIAL): Sequential VB unload file containing all DBPAUTP0 database segments
- **SYSUT1** (FILE_SEQUENTIAL): Target dataset for deletion (same DSN as DFSURGU1)

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of DBPAUTP0
- Understand business rules implemented in DBPAUTP0
- Trace program calls from DBPAUTP0
- Identify inputs/outputs for DBPAUTP0
- Maintain or modify DBPAUTP0

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.