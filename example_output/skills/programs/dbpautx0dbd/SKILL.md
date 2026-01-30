---
name: dbpautx0dbd
description: "This file contains the source for the IMS Database Definition (DBD) of DBPAUTX0, an indexed VSAM-protected database. It defines one dataset group DSG001 with dataset DDPAUTX0 of size 4096. It specifies a single root segment PAUTINDX (6 bytes) with unique sequential packed key INDXSEQ and a logical child to PAUTSUM0 in DBPAUTP0 via index ACCNTID."
---

# DBPAUTX0

**Type:** OTHER (UTILITY)

## Purpose

This file contains the source for the IMS Database Definition (DBD) of DBPAUTX0, an indexed VSAM-protected database. It defines one dataset group DSG001 with dataset DDPAUTX0 of size 4096. It specifies a single root segment PAUTINDX (6 bytes) with unique sequential packed key INDXSEQ and a logical child to PAUTSUM0 in DBPAUTP0 via index ACCNTID.

## Business Rules

- **BR001**: Unique sequential key INDXSEQ ensures each PAUTINDX segment is uniquely identified and maintained in sequence order
- **BR002**: Segment PAUTINDX has an expected frequency of 100000 occurrences for performance and space planning
- **BR003**: Logical child relationship to PAUTSUM0 segment in database DBPAUTP0 accessed via index ACCNTID

## Inputs

- **DDPAUTX0** (FILE_VSAM): Primary dataset referenced in dataset group DSG001 with size 4096

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of DBPAUTX0
- Understand business rules implemented in DBPAUTX0
- Identify inputs/outputs for DBPAUTX0
- Maintain or modify DBPAUTX0

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.