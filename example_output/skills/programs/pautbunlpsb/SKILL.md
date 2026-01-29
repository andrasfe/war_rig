---
name: pautbunlpsb
description: "This PSB (Program Specification Block) defines the database access parameters for an IMS (Information Management System) application. It specifies the database (DBPAUTP0), the processing options (PROCOPT=GOTP), key length, and the segments (PAUTSUM0, PAUTDTL1) that the program can access. The PSB is designed for COBOL and is named PAUTBUNL."
---

# PAUTBUNL

**Type:** OTHER (UTILITY)

## Purpose

This PSB (Program Specification Block) defines the database access parameters for an IMS (Information Management System) application. It specifies the database (DBPAUTP0), the processing options (PROCOPT=GOTP), key length, and the segments (PAUTSUM0, PAUTDTL1) that the program can access. The PSB is designed for COBOL and is named PAUTBUNL.

## Inputs

- **DBPAUTP0** (IMS_SEGMENT): The IMS database accessed by the program, as defined by DBDNAME.
- **PAUTSUM0** (IMS_SEGMENT): The PAUTSUM0 segment within the DBPAUTP0 database.
- **PAUTDTL1** (IMS_SEGMENT): The PAUTDTL1 segment within the DBPAUTP0 database, a child segment of PAUTSUM0.

## Outputs

- **DBPAUTP0** (IMS_SEGMENT): The IMS database accessed by the program. Due to PROCOPT=GOTP, updates are possible.
- **PAUTSUM0** (IMS_SEGMENT): The PAUTSUM0 segment within the DBPAUTP0 database. Updates are possible due to PROCOPT=GOTP.
- **PAUTDTL1** (IMS_SEGMENT): The PAUTDTL1 segment within the DBPAUTP0 database, a child segment of PAUTSUM0. Updates are possible due to PROCOPT=GOTP.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PAUTBUNL
- Identify inputs/outputs for PAUTBUNL
- Maintain or modify PAUTBUNL

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.