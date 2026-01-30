# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-30 19:40:27.088006

## Purpose

This BMS mapset defines the Pending Authorization Screen (COPAU0A) for the CardDemo application. It provides a user interface to search pending authorizations by Account ID, display customer details including name, address, status, phone, limits, balances, approval/decline counts, and a scrollable list of up to 5 transactions with fields for selection, transaction ID, date, time, type, A/D, status, and amount. Users interact via input fields and PF keys for navigation.

**Business Context**: Serves credit card authorization processing by allowing operators to view, select, and navigate pending authorization transactions for an account.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Unprotected input field for entering Account ID to search for pending authorizations |
| SEL0001 | IOType.CICS_MAP | Unprotected selection field (Sel) for first transaction in list |
| SEL0002 | IOType.CICS_MAP | Unprotected selection field (Sel) for second transaction in list |
| SEL0003 | IOType.CICS_MAP | Unprotected selection field (Sel) for third transaction in list |
| SEL0004 | IOType.CICS_MAP | Unprotected selection field (Sel) for fourth transaction in list |
| SEL0005 | IOType.CICS_MAP | Unprotected selection field (Sel) for fifth transaction in list |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | Primary map displaying screen header (Tran, Date, Prog, Time), title, search prompt, customer info (name, ID, address, status, phone), limits/balances/amounts (credit/cash lim/bal, appr/decl amt), transaction list headers and data rows, instructions, error message, and PF key help |
| ERRMSG | IOType.CICS_MAP | Error message display field with bright and red attributes |

## Business Rules

- **BR001**: User must type 'S' in one of the SEL fields to select and view details of a specific authorization from the transaction list
- **BR002**: Screen navigation uses PF keys: ENTER to continue, F3 to go back, F7 for backward scroll, F8 for forward scroll

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

- ? Which CICS COBOL program(s) use this BMS mapset?
  - Context: BMS file defines the map but does not reference calling programs
- ? What is the associated CICS transaction ID for this screen?
  - Context: Not defined in BMS file
