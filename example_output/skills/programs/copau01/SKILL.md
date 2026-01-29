---
name: copau01
description: "This copybook defines the input and output data structures for a CICS BMS map named COPAU1. It contains field definitions for transaction details, dates, times, card numbers, authorization information, merchant details, and error messages, used for both input (COPAU1AI) and output (COPAU1AO) layouts."
---

# COPAU01

**Type:** COPYBOOK (BMS)
**Context:** UNKNOWN

## Purpose

This copybook defines the input and output data structures for a CICS BMS map named COPAU1. It contains field definitions for transaction details, dates, times, card numbers, authorization information, merchant details, and error messages, used for both input (COPAU1AI) and output (COPAU1AO) layouts.

## Inputs

- **COPAU1AI** (CICS_MAP): Input data structure for the COPAU1 CICS BMS map, containing fields for transaction name, titles, current date and time, card number, authorization details, merchant information, and error messages.

## Outputs

- **COPAU1AO** (CICS_MAP): Output data structure for the COPAU1 CICS BMS map, containing fields for transaction name, titles, current date and time, card number, authorization details, merchant information, and error messages, formatted for display.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAU01
- Identify inputs/outputs for COPAU01
- Maintain or modify COPAU01

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.