# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-02-27 14:43:28.976979

## Purpose

This BMS map defines the screen layout for the 'CardDemo - Pending Authorization Screen'. It is used to display pending credit card authorizations and related customer information, allowing users to search by account ID and select transactions for further action.

**Business Context**: This screen is part of a credit card authorization workflow, likely used by customer service representatives or fraud analysts to review and manage pending transactions.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID entered by the user to search for pending authorizations. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | The entire screen layout, including static text, customer information, and a list of pending transactions. |

## Paragraphs/Procedures

### ~~COPAU00~~ (Dead Code)
*Artifact 'COPAU00' (map) is never referenced by any other artifact in the dependency graph*

### ~~COPAU0A~~ (Dead Code)
*Screen/Map 'COPAU0A' is never sent to or received from by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU00 | map | 19 | Artifact 'COPAU00' (map) is never referenced by any other artifact in the dependency graph |
| COPAU0A | screen | 26 | Screen/Map 'COPAU0A' is never sent to or received from by any program |

## Open Questions

- ? What is the purpose of the '&&SYSPARM' variable used in the TYPE parameter of the DFHMSD macro?
  - Context: The code uses 'TYPE=&&SYSPARM' without explaining where this variable is defined or what values it can take.
