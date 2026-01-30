---
name: unldpadb
description: "This JCL job first deletes any existing sequential output files from prior runs using IEFBR14, then executes the IMS Database Unload utility DFSRRC00 to unload the PAUTHDB and PAUTHDBX IMS databases into sequential files ROOT.FILEO and CHILD.FILEO. The unload targets root and child segments based on output file naming and PARM specifications (PAUDBUNL, PAUTBUNL). It supports the CARDDEMO application by creating flat files from the hierarchical IMS database."
---

# UNLDPADB

**Type:** JCL (BATCH)
**Context:** Unloading IMS PAUTDB/PAUTHDB database for backup, migration, or processing in the AWS.M2.CARDDEMO application

## Purpose

This JCL job first deletes any existing sequential output files from prior runs using IEFBR14, then executes the IMS Database Unload utility DFSRRC00 to unload the PAUTHDB and PAUTHDBX IMS databases into sequential files ROOT.FILEO and CHILD.FILEO. The unload targets root and child segments based on output file naming and PARM specifications (PAUDBUNL, PAUTBUNL). It supports the CARDDEMO application by creating flat files from the hierarchical IMS database.

## Called Programs

- IEFBR14 (STATIC_CALL)
- DFSRRC00 (STATIC_CALL)

## Inputs

- **DDPAUTP0** (IMS_SEGMENT): IMS PAUTHDB database (primary/root segments for unload)
- **DDPAUTX0** (IMS_SEGMENT): IMS PAUTHDBX database (secondary/child segments for unload)
- **DD1** (FILE_SEQUENTIAL): Prior ROOT.FILEO for deletion if exists
- **DD2** (FILE_SEQUENTIAL): Prior CHILD.FILEO for deletion if exists

## Outputs

- **OUTFIL1** (FILE_SEQUENTIAL): Unloaded root segments from PAUTHDB in fixed-block format (LRECL=100)
- **OUTFIL2** (FILE_SEQUENTIAL): Unloaded child segments from PAUTHDBX in fixed-block format (LRECL=206)

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of UNLDPADB
- Trace program calls from UNLDPADB
- Identify inputs/outputs for UNLDPADB
- Maintain or modify UNLDPADB

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.