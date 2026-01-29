---
name: copau00bms
description: "This BMS map defines the screen layout for the CardDemo Pending Authorization screen. It displays customer account information and a list of pending transactions, allowing the user to select a transaction for further details."
---

# COPAU00

**Type:** BMS (ONLINE_CICS)
**Context:** This screen is used to view and manage pending authorizations for credit card transactions.

## Purpose

This BMS map defines the screen layout for the CardDemo Pending Authorization screen. It displays customer account information and a list of pending transactions, allowing the user to select a transaction for further details.

## Inputs

- **ACCTID** (CICS_MAP): Account ID entered by the user to search for pending authorizations.
- **SEL0001 - SEL0005** (CICS_MAP): Selection fields for each transaction displayed on the screen. User enters a value to select a specific transaction.

## Outputs

- **COPAU0A** (CICS_MAP): The entire screen layout, including labels, data fields, and messages. Displays customer information and pending transactions.
- **TRNNAME** (CICS_MAP): Transaction Name
- **TITLE01** (CICS_MAP): Title 1
- **CURDATE** (CICS_MAP): Current Date
- **PGMNAME** (CICS_MAP): Program Name
- *(+24 more outputs)*

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAU00
- Identify inputs/outputs for COPAU00
- Maintain or modify COPAU00

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.