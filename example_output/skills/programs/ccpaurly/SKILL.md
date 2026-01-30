---
name: ccpaurly
description: "This COBOL copybook defines data structures at level 05 for a Pending Authorization Response record. It includes elementary fields for card number (PIC X(16)), transaction ID (PIC X(15)), authorization ID code (PIC X(06)), authorization response code (PIC X(02)), response reason (PIC X(04)), and approved amount (PIC +9(10).99). The fields share a 'PA-RL-' prefix, logically grouping them for use in payment authorization processing."
---

# CCPAURLY

**Type:** COPYBOOK (UTILITY)
**Context:** Payment card authorization processing, capturing response details from authorization requests

## Purpose

This COBOL copybook defines data structures at level 05 for a Pending Authorization Response record. It includes elementary fields for card number (PIC X(16)), transaction ID (PIC X(15)), authorization ID code (PIC X(06)), authorization response code (PIC X(02)), response reason (PIC X(04)), and approved amount (PIC +9(10).99). The fields share a 'PA-RL-' prefix, logically grouping them for use in payment authorization processing.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of CCPAURLY
- Maintain or modify CCPAURLY

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.