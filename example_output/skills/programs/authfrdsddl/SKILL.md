---
name: authfrdsddl
description: "This DDL script creates a DB2 table named CARDDEMO.AUTHFRDS to store authorization and fraud-related details for card transactions. The table includes fields for card details, authorization timestamps, merchant information, transaction amounts, and fraud indicators such as AUTH_FRAUD and FRAUD_RPT_DATE. It supports fraud detection and reporting in a card processing environment."
---

# AUTHFRDS

**Type:** OTHER (UTILITY)
**Context:** Card authorization fraud logging and analysis for payment processing systems

## Purpose

This DDL script creates a DB2 table named CARDDEMO.AUTHFRDS to store authorization and fraud-related details for card transactions. The table includes fields for card details, authorization timestamps, merchant information, transaction amounts, and fraud indicators such as AUTH_FRAUD and FRAUD_RPT_DATE. It supports fraud detection and reporting in a card processing environment.

## Business Rules

- **BR001**: CARD_NUM and AUTH_TS form a composite primary key to ensure uniqueness of each authorization record per card

## Outputs

- **CARDDEMO.AUTHFRDS** (DB2_TABLE): Table storing card authorization details including fraud indicators, merchant data, and transaction information

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of AUTHFRDS
- Understand business rules implemented in AUTHFRDS
- Identify inputs/outputs for AUTHFRDS
- Maintain or modify AUTHFRDS

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.