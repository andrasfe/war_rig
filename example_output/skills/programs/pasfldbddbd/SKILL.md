---
name: pasfldbddbd
description: "This DBDGEN source file defines the IMS database named PASFLDBD using GSAM and BSAM access methods without password protection. It specifies a single dataset group DSG001 with input dataset DD1=PASFILIP and output dataset DD2=PASFILOP, both using fixed-length records (RECFM=F) of 100 bytes. The definition was generated on 04/21/2023 for IMS version 15.1."
---

# PASFLDBD

**Type:** OTHER (UTILITY)

## Purpose

This DBDGEN source file defines the IMS database named PASFLDBD using GSAM and BSAM access methods without password protection. It specifies a single dataset group DSG001 with input dataset DD1=PASFILIP and output dataset DD2=PASFILOP, both using fixed-length records (RECFM=F) of 100 bytes. The definition was generated on 04/21/2023 for IMS version 15.1.

## Inputs

- **PASFILIP** (FILE_SEQUENTIAL): Input dataset (DD1) for the PASFLDBD GSAM database, referenced in dataset group DSG001

## Outputs

- **PASFILOP** (FILE_SEQUENTIAL): Output dataset (DD2) for the PASFLDBD GSAM database, referenced in dataset group DSG001

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PASFLDBD
- Identify inputs/outputs for PASFLDBD
- Maintain or modify PASFLDBD

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.