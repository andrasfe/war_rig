# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-30 19:45:36.664528

## Purpose

Defines BMS mapset COPAU01 with map COPAU1A for the 'Pending Authorization Details Screen' in CardDemo application. Displays authorization details including card number, auth date/time/response, amount, merchant info, fraud status, and supports function keys for navigation and actions like marking fraud. Used in CICS online transactions for viewing/acting on pending credit card authorizations.

**Business Context**: Credit card processing demonstration (CardDemo), specifically for reviewing pending authorization details to detect/support fraud management.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | CICS map for receiving user input such as function key presses (F3=Back, F5=Mark/Remove Fraud, F8=Next Auth) and any modifiable fields |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | CICS map for sending screen data including transaction details, auth info (AUTHDT, AUTHTM, AUTHRSP, etc.), merchant details (MERNAME, MERID, etc.), and error messages |

## Paragraphs/Procedures

### ~~COPAU01~~ (Dead Code)
*Artifact 'COPAU01' (map) is never referenced by any other artifact in the dependency graph*

### ~~COPAU1A~~ (Dead Code)
*Screen/Map 'COPAU1A' is never sent to or received from by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU01 | map | 19 | Artifact 'COPAU01' (map) is never referenced by any other artifact in the dependency graph |
| COPAU1A | screen | 26 | Screen/Map 'COPAU1A' is never sent to or received from by any program |

## Open Questions

- ? Which specific CICS transaction/program uses this mapset COPAU01?
  - Context: BMS file references CardDemo (line 2) but does not name the calling program or transaction ID
- ? Are data fields (e.g., CARDNUM, AUTHDT) intended as input (unprotected) or display-only?
  - Context: No explicit PROT attribute on data fields (e.g., lines 85-87 for CARDNUM); labels use ASKIP but data fields do not specify PROT
