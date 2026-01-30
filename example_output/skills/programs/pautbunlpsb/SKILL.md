---
name: pautbunlpsb
description: "This file defines an IMS Program Specification Block (PSB) named PAUTBUNL for a COBOL language program. It specifies a single PCB named PAUTBPCB providing access to the IMS database DBDNAME=DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14. The PCB defines two sensegments: PAUTSUM0 as the root segment (PARENT=0) and PAUTDTL1 as a child segment under PAUTSUM0."
---

# PAUTBUNL

**Type:** OTHER (BATCH)

## Purpose

This file defines an IMS Program Specification Block (PSB) named PAUTBUNL for a COBOL language program. It specifies a single PCB named PAUTBPCB providing access to the IMS database DBDNAME=DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14. The PCB defines two sensegments: PAUTSUM0 as the root segment (PARENT=0) and PAUTDTL1 as a child segment under PAUTSUM0.

## Inputs

- **DBPAUTP0** (IMS_SEGMENT): IMS database referenced by the PCB, accessible via segments PAUTSUM0 (root) and PAUTDTL1 (child)

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PAUTBUNL
- Identify inputs/outputs for PAUTBUNL
- Maintain or modify PAUTBUNL

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.