# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: COPYBOOK
**Analyzed**: 2026-01-28 14:55:24.290509

## Purpose

This copybook defines the data structure COPAU0AI, which appears to be used for screen mapping in a CICS environment. It contains fields for transaction names, titles, dates, times, account IDs, customer information, addresses, phone numbers, and error messages. It also redefines COPAU0AI as COPAU0AO, providing an alternate layout for the same data.

**Business Context**: UNKNOWN

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AI | CICS_MAP | Data structure for screen input/output, containing fields for transaction name, titles, current date and time, account ID, customer name, customer ID, address, account status, phone number, and amounts. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AI | CICS_MAP | Data structure for screen input/output, containing fields for transaction name, titles, current date and time, account ID, customer name, customer ID, address, account status, phone number, and amounts. |

## Paragraphs/Procedures

### COPAU00
[Citadel] Paragraph identified by static analysis

### ~~COPAU0AI~~ (Dead Code)
*Record layout 'COPAU0AI' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU0AI | record_layout | 1 | Record layout 'COPAU0AI' is never used by any program |

## Open Questions

- ? What is the specific purpose of each field within the COPAU0AI and COPAU0AO structures?
  - Context: The code only defines the data structures, not their usage.
- ? What CICS transactions or programs use this copybook?
  - Context: The copybook itself does not provide this information.
