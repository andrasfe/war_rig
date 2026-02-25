# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-02-25 15:30:57.423429

## Purpose

Defines the COPAU00 CICS mapset with COPAU0A map for the Pending Authorization Screen in CardDemo application. Allows searching pending authorizations by account ID, displays customer details (name, ID, address, phone, status, approval/decline counts, credit/cash limits and balances), and lists up to 5 transactions with selection options. Includes headers, timestamps, error message field, and function key instructions.

**Business Context**: Credit card authorization management in CICS online transaction processing for viewing and selecting pending authorizations.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Unprotected input field for entering account ID to search for pending authorizations |
| SEL0001 | IOType.CICS_MAP | Unprotected selection field for first transaction (e.g., enter 'S' to view details) |
| SEL0002 | IOType.CICS_MAP | Unprotected selection field for second transaction |
| SEL0003 | IOType.CICS_MAP | Unprotected selection field for third transaction |
| SEL0004 | IOType.CICS_MAP | Unprotected selection field for fourth transaction |
| SEL0005 | IOType.CICS_MAP | Unprotected selection field for fifth transaction |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | Display map containing customer information, limits, balances, transaction list, headers, timestamps, and instructions |
| ERRMSG | IOType.CICS_MAP | Field for displaying error messages to the user |

## Business Rules

- **BR001**: User input fields such as ACCTID and SELxxxx are unprotected (UNPROT) to accept search criteria and transaction selections, with validation expected in the calling CICS program.
- **BR002**: Display-only fields use ASKIP and NORM/PROT attributes to prevent user modification.

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

- ? Which CICS COBOL program(s) reference and use this COPAU00 BMS mapset?
  - Context: BMS file defines the map but does not specify calling programs.
- ? What are the exact validation rules for inputs like ACCTID length/format or SEL values?
  - Context: BMS defines layout and attributes but no programmatic validation.
