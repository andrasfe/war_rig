---
name: copau00
description: "This copybook defines the data structure COPAU0AI, which appears to be used for screen mapping in a CICS environment. It contains fields for transaction names, titles, dates, times, account IDs, customer information, addresses, phone numbers, and error messages. It also redefines COPAU0AI as COPAU0AO, providing an alternate layout for the same data."
---

# COPAU00

**Type:** COPYBOOK (UNKNOWN)
**Context:** UNKNOWN

## Purpose

This copybook defines the data structure COPAU0AI, which appears to be used for screen mapping in a CICS environment. It contains fields for transaction names, titles, dates, times, account IDs, customer information, addresses, phone numbers, and error messages. It also redefines COPAU0AI as COPAU0AO, providing an alternate layout for the same data.

## Inputs

- **COPAU0AI** (CICS_MAP): Data structure for screen input/output, containing fields for transaction name, titles, current date and time, account ID, customer name, customer ID, address, account status, phone number, and amounts.

## Outputs

- **COPAU0AI** (CICS_MAP): Data structure for screen input/output, containing fields for transaction name, titles, current date and time, account ID, customer name, customer ID, address, account status, phone number, and amounts.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAU00
- Identify inputs/outputs for COPAU00
- Maintain or modify COPAU00

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.