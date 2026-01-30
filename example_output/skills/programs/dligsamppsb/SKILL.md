---
name: dligsamppsb
description: "This file is the source for generating the IMS Program Specification Block (PSB) named DLIGSAMP for use in a COBOL program. It defines a database PCB (PAUTBPCB) for the hierarchical IMS database DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14, providing access to root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0). It also defines two GSAM PCBs for databases PASFLDBD and PADFLDBD, both with PROCOPT=LS."
---

# DLIGSAMP

**Type:** OTHER (UTILITY)

## Purpose

This file is the source for generating the IMS Program Specification Block (PSB) named DLIGSAMP for use in a COBOL program. It defines a database PCB (PAUTBPCB) for the hierarchical IMS database DBPAUTP0 with PROCOPT=GOTP and KEYLEN=14, providing access to root segment PAUTSUM0 (PARENT=0) and child segment PAUTDTL1 (PARENT=PAUTSUM0). It also defines two GSAM PCBs for databases PASFLDBD and PADFLDBD, both with PROCOPT=LS.

## Inputs

- **DBPAUTP0** (IMS_SEGMENT): Hierarchical IMS database accessed via PCB PAUTBPCB with segments PAUTSUM0 (root) and PAUTDTL1
- **PASFLDBD** (IMS_SEGMENT): GSAM IMS database accessed via unnamed PCB
- **PADFLDBD** (IMS_SEGMENT): GSAM IMS database accessed via unnamed PCB

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of DLIGSAMP
- Identify inputs/outputs for DLIGSAMP
- Maintain or modify DLIGSAMP

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.