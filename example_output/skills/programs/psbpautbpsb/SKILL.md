---
name: psbpautbpsb
description: Defines the Program Specification Block (PSB) PSBPAUTB for a COBOL application program using IMS DL/I calls. Specifies a single database PCB named PAUTBPCB for accessing DBD DBPAUTP0 with PROCOPT=AP and KEYLEN=14. Includes root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 under PAUTSUM0.
---

# PSBPAUTB

**Type:** OTHER (BATCH)
**Context:** IMS database access configuration for batch DL/I programs interacting with the PAUTP0 database hierarchy

## Purpose

Defines the Program Specification Block (PSB) PSBPAUTB for a COBOL application program using IMS DL/I calls. Specifies a single database PCB named PAUTBPCB for accessing DBD DBPAUTP0 with PROCOPT=AP and KEYLEN=14. Includes root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 under PAUTSUM0.

## Inputs

- **PAUTSUM0** (IMS_SEGMENT): Root segment (PARENT=0) accessible via PAUTBPCB PCB
- **PAUTDTL1** (IMS_SEGMENT): Child segment with PARENT=PAUTSUM0 accessible via PAUTBPCB PCB

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PSBPAUTB
- Identify inputs/outputs for PSBPAUTB
- Maintain or modify PSBPAUTB

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.