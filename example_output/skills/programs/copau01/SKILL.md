---
name: copau01
description: "This copybook defines the COBOL data structures for the COPAU01 BMS map used in CICS online transactions. It provides the input area COPAU1AI with length, format/attribute, and data fields for screen elements including transaction names, titles, dates, times, card numbers, authorization details, merchant information, and error messages. The output area COPAU1AO redefines COPAU1AI with BMS attribute fields (C/P/H/V) and output data fields for screen rendering."
---

# COPAU01

**Type:** COPYBOOK (ONLINE_CICS)
**Context:** Supports credit card authorization screens in payment processing, displaying and capturing transaction, card, authorization response, and merchant details.

## Purpose

This copybook defines the COBOL data structures for the COPAU01 BMS map used in CICS online transactions. It provides the input area COPAU1AI with length, format/attribute, and data fields for screen elements including transaction names, titles, dates, times, card numbers, authorization details, merchant information, and error messages. The output area COPAU1AO redefines COPAU1AI with BMS attribute fields (C/P/H/V) and output data fields for screen rendering.

## Inputs

- **COPAU1AI** (CICS_MAP): Input map structure containing length (L), format/attribute (F/A), and input data (I) fields for all screen elements

## Outputs

- **COPAU1AO** (CICS_MAP): Output map structure redefining input, with changed/protected/highlighted/unprotected attributes (C/P/H/V) and output data (O) fields for screen display

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAU01
- Identify inputs/outputs for COPAU01
- Maintain or modify COPAU01

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.