---
name: ccpaurqy
description: "This COBOL copybook defines a level 05 group structure for Pending Authorization Request data (PA-RQ-* fields), to be subordinated under a level 01 record in including programs. It includes fields for authorization timestamp, card details, transaction amount, merchant information, and transaction ID, supporting credit card processing workflows. No executable logic is present; it serves solely as a data layout definition."
---

# CCPAURQY

**Type:** COPYBOOK (UTILITY)
**Context:** Payment authorization processing for card transactions, capturing details from POS or acquirer requests (inferred from field names like PA-RQ-CARD-NUM, PA-RQ-TRANSACTION-AMT, PA-RQ-MERCHANT-ID on lines 21,27,31).

## Purpose

This COBOL copybook defines a level 05 group structure for Pending Authorization Request data (PA-RQ-* fields), to be subordinated under a level 01 record in including programs. It includes fields for authorization timestamp, card details, transaction amount, merchant information, and transaction ID, supporting credit card processing workflows. No executable logic is present; it serves solely as a data layout definition.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of CCPAURQY
- Maintain or modify CCPAURQY

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.