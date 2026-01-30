---
name: padfldbddbd
description: "This DBD source file defines the IMS database PADFLDBD as a GSAM database with BSAM access method and no password protection. It specifies Dataset Group 1 (DSG001) with input dataset DD1=PADFILIP and output dataset DD2=PADFILOP, both using fixed-length records of 200 bytes (RECFM=F). The definition was prepared for DBDGEN processing on IMS version 15.1."
---

# PADFLDBD

**Type:** OTHER (UTILITY)

## Purpose

This DBD source file defines the IMS database PADFLDBD as a GSAM database with BSAM access method and no password protection. It specifies Dataset Group 1 (DSG001) with input dataset DD1=PADFILIP and output dataset DD2=PADFILOP, both using fixed-length records of 200 bytes (RECFM=F). The definition was prepared for DBDGEN processing on IMS version 15.1.

## Inputs

- **PADFILIP** (FILE_SEQUENTIAL): Input dataset (DD1) associated with Dataset Group 1 of the PADFLDBD GSAM database, fixed 200-byte records

## Outputs

- **PADFILOP** (FILE_SEQUENTIAL): Output dataset (DD2) associated with Dataset Group 1 of the PADFLDBD GSAM database, fixed 200-byte records

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PADFLDBD
- Identify inputs/outputs for PADFLDBD
- Maintain or modify PADFLDBD

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.