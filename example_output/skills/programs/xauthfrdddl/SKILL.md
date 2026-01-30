---
name: xauthfrdddl
description: This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index keys on CARD_NUM in ascending order and AUTH_TS in descending order. The COPY YES option is specified to allow the index to participate in image copy operations.
---

# XAUTHFRD

**Type:** OTHER (UTILITY)

## Purpose

This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index keys on CARD_NUM in ascending order and AUTH_TS in descending order. The COPY YES option is specified to allow the index to participate in image copy operations.

## Business Rules

- **BR001**: Enforces uniqueness on the combination of CARD_NUM and AUTH_TS

## Inputs

- **CARDDEMO.AUTHFRDS** (DB2_TABLE): DB2 table on which the unique index is created

## Outputs

- **CARDDEMO.XAUTHFRD** (OTHER): Unique DB2 index created on AUTHFRDS table

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of XAUTHFRD
- Understand business rules implemented in XAUTHFRD
- Identify inputs/outputs for XAUTHFRD
- Maintain or modify XAUTHFRD

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.