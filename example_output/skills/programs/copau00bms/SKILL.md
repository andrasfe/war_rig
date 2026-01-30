---
name: copau00bms
description: "This BMS mapset defines the Pending Authorization Screen (COPAU0A) for the CardDemo application. It provides a user interface to search pending authorizations by Account ID, display customer details including name, address, status, phone, limits, balances, approval/decline counts, and a scrollable list of up to 5 transactions with fields for selection, transaction ID, date, time, type, A/D, status, and amount. Users interact via input fields and PF keys for navigation."
---

# COPAU00

**Type:** BMS (ONLINE_CICS)
**Context:** Serves credit card authorization processing by allowing operators to view, select, and navigate pending authorization transactions for an account.

## Purpose

This BMS mapset defines the Pending Authorization Screen (COPAU0A) for the CardDemo application. It provides a user interface to search pending authorizations by Account ID, display customer details including name, address, status, phone, limits, balances, approval/decline counts, and a scrollable list of up to 5 transactions with fields for selection, transaction ID, date, time, type, A/D, status, and amount. Users interact via input fields and PF keys for navigation.

## Business Rules

- **BR001**: User must type 'S' in one of the SEL fields to select and view details of a specific authorization from the transaction list
- **BR002**: Screen navigation uses PF keys: ENTER to continue, F3 to go back, F7 for backward scroll, F8 for forward scroll

## Inputs

- **ACCTID** (CICS_MAP): Unprotected input field for entering Account ID to search for pending authorizations
- **SEL0001** (CICS_MAP): Unprotected selection field (Sel) for first transaction in list
- **SEL0002** (CICS_MAP): Unprotected selection field (Sel) for second transaction in list
- **SEL0003** (CICS_MAP): Unprotected selection field (Sel) for third transaction in list
- **SEL0004** (CICS_MAP): Unprotected selection field (Sel) for fourth transaction in list
- *(+1 more inputs)*

## Outputs

- **COPAU0A** (CICS_MAP): Primary map displaying screen header (Tran, Date, Prog, Time), title, search prompt, customer info (name, ID, address, status, phone), limits/balances/amounts (credit/cash lim/bal, appr/decl amt), transaction list headers and data rows, instructions, error message, and PF key help
- **ERRMSG** (CICS_MAP): Error message display field with bright and red attributes

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAU00
- Understand business rules implemented in COPAU00
- Identify inputs/outputs for COPAU00
- Maintain or modify COPAU00

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.