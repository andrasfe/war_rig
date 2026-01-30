---
name: dbpautp0dbd
description: "This DBD source file defines the IMS HIDAM database DBPAUTP0 using VSAM access for pending authorization data. It specifies dataset DDPAUTP0, root segment PAUTSUM0 (100 bytes) with unique sequential key ACCNTID, and child segment PAUTDTL1 (200 bytes) with unique sequential key PAUT9CTS parented by PAUTSUM0. The file is processed by the DBDGEN utility to generate the DBD control block."
---

# DBPAUTP0

**Type:** OTHER (UTILITY)
**Context:** Stores summary and detail records for pending account authorizations, supporting hierarchical access via root account ID.

## Purpose

This DBD source file defines the IMS HIDAM database DBPAUTP0 using VSAM access for pending authorization data. It specifies dataset DDPAUTP0, root segment PAUTSUM0 (100 bytes) with unique sequential key ACCNTID, and child segment PAUTDTL1 (200 bytes) with unique sequential key PAUT9CTS parented by PAUTSUM0. The file is processed by the DBDGEN utility to generate the DBD control block.

## Business Rules

- **BR001**: Root segment PAUTSUM0 is uniquely sequenced by ACCNTID field
- **BR002**: Child segment PAUTDTL1 is parented exclusively by root segment PAUTSUM0
- **BR003**: Child segment PAUTDTL1 is uniquely sequenced by PAUT9CTS field

## Outputs

- **DDPAUTP0** (FILE_VSAM): VSAM dataset hosting the HIDAM database segments
- **PAUTSUM0** (IMS_SEGMENT): Root segment containing pending authorization summary data, 100 bytes
- **PAUTDTL1** (IMS_SEGMENT): Child segment containing pending authorization detail data, 200 bytes

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of DBPAUTP0
- Understand business rules implemented in DBPAUTP0
- Identify inputs/outputs for DBPAUTP0
- Maintain or modify DBPAUTP0

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.