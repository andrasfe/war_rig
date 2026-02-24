# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-02-24 17:37:22.671384

## Purpose

This BMS map defines the screen layout for the 'View Authorizations' function within the CardDemo application. It displays customer account information and a list of recent transactions for approval or decline.

**Business Context**: This screen is used by customer service representatives or fraud analysts to review and manage pending card authorizations.

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | Represents the CICS screen layout for displaying pending authorizations. Includes fields for account ID, customer name, address, transaction details, and approval/decline options. |

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
  - Context: The meaning and usage of this variable are unclear from the code.
