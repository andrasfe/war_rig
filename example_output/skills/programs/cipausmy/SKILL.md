---
name: cipausmy
description: "This copybook defines the data structure for the IMS segment 'PENDING AUTHORIZATION SUMMARY'. It includes fields for account ID (PA-ACCT-ID), customer ID (PA-CUST-ID), authorization status (PA-AUTH-STATUS), an array of account statuses (PA-ACCOUNT-STATUS OCCURS 5 TIMES), credit and cash limits and balances, counts of approved/declined authorizations, and corresponding amounts. A filler field pads the structure to the required length."
---

# CIPAUSMY

**Type:** COPYBOOK (UTILITY)
**Context:** Supports management of pending authorizations for customer accounts in an IMS database, tracking statuses, limits, balances, and authorization history for credit and cash transactions.

## Purpose

This copybook defines the data structure for the IMS segment 'PENDING AUTHORIZATION SUMMARY'. It includes fields for account ID (PA-ACCT-ID), customer ID (PA-CUST-ID), authorization status (PA-AUTH-STATUS), an array of account statuses (PA-ACCOUNT-STATUS OCCURS 5 TIMES), credit and cash limits and balances, counts of approved/declined authorizations, and corresponding amounts. A filler field pads the structure to the required length.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of CIPAUSMY
- Maintain or modify CIPAUSMY

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.