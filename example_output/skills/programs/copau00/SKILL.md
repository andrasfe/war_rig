---
name: copau00
description: "This COBOL copybook defines the BMS (Basic Mapping Support) map structure COPAU0AI for CICS online screens, supporting display and input of customer account information including account ID, name, address, status, phone, approval/decline counts, credit/cash limits and balances, and up to five selectable recent transactions. COPAU0AI provides input fields (marked I) for terminal receive and output preparation, while the redefines COPAU0AO provides output fields (marked O) with BMS attributes (C=changed, P=protected, H=highlight, V=visible). It is used in CICS programs for account inquiry/update screens."
---

# COPAU00

**Type:** COPYBOOK (ONLINE_CICS)
**Context:** Customer account profile display and transaction history selection in a financial or credit card processing system.

## Purpose

This COBOL copybook defines the BMS (Basic Mapping Support) map structure COPAU0AI for CICS online screens, supporting display and input of customer account information including account ID, name, address, status, phone, approval/decline counts, credit/cash limits and balances, and up to five selectable recent transactions. COPAU0AI provides input fields (marked I) for terminal receive and output preparation, while the redefines COPAU0AO provides output fields (marked O) with BMS attributes (C=changed, P=protected, H=highlight, V=visible). It is used in CICS programs for account inquiry/update screens.

## Inputs

- **COPAU0AI** (CICS_MAP): Primary BMS map structure for receiving input from CICS terminal, including selection fields SEL0001I to SEL0005I (PIC X(1), lines 150,198,246,294,384), transaction details, account data, and error message ERRMSGI (PIC X(78), line 390)

## Outputs

- **COPAU0AO** (CICS_MAP): Redefined BMS map structure for sending output to CICS terminal, including account details like ACCTIDO (PIC X(11), line 434), balances like CREDBALO (PIC X(12), line 506), transaction history like TRNID01O (PIC X(16), line 530), and error message ERRMSGO (PIC X(78), line 764)

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAU00
- Identify inputs/outputs for COPAU00
- Maintain or modify COPAU00

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.