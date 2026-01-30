---
name: copau01bms
description: "Defines BMS mapset COPAU01 with map COPAU1A for the 'Pending Authorization Details Screen' in CardDemo application. Displays authorization details including card number, auth date/time/response, amount, merchant info, fraud status, and supports function keys for navigation and actions like marking fraud. Used in CICS online transactions for viewing/acting on pending credit card authorizations."
---

# COPAU01

**Type:** BMS (ONLINE_CICS)
**Context:** Credit card processing demonstration (CardDemo), specifically for reviewing pending authorization details to detect/support fraud management.

## Purpose

Defines BMS mapset COPAU01 with map COPAU1A for the 'Pending Authorization Details Screen' in CardDemo application. Displays authorization details including card number, auth date/time/response, amount, merchant info, fraud status, and supports function keys for navigation and actions like marking fraud. Used in CICS online transactions for viewing/acting on pending credit card authorizations.

## Inputs

- **COPAU1A** (CICS_MAP): CICS map for receiving user input such as function key presses (F3=Back, F5=Mark/Remove Fraud, F8=Next Auth) and any modifiable fields

## Outputs

- **COPAU1A** (CICS_MAP): CICS map for sending screen data including transaction details, auth info (AUTHDT, AUTHTM, AUTHRSP, etc.), merchant details (MERNAME, MERID, etc.), and error messages

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAU01
- Identify inputs/outputs for COPAU01
- Maintain or modify COPAU01

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.