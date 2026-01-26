# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-26 02:33:04.990818

## Purpose

This BMS mapset defines the COPAU01 screen for the CardDemo application, displaying pending authorization details including card number, auth date/time/response, amount, merchant info, and fraud/match status. The screen supports view-only display with protected fields and function keys for navigation (F3=Back), fraud marking (F5), and next auth (F8). It is designed for CICS online transaction review in a payment processing demo.

**Business Context**: Review and management of pending credit card authorizations, including fraud detection and merchant details verification.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | Receives user input from the terminal, primarily function key actions (F3, F5, F8) since all data fields are protected with ASKIP; ERRMSG field supports error feedback but remains protected. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | Defines the 24x80 screen layout populated by the calling CICS program with authorization and merchant details for display to the user. |

## Business Rules

- **BR001**: All data display fields are protected to enforce view-only access, preventing user modification of authorization details.
- **BR002**: Error messages are displayed in a dedicated protected field with bright and set attributes for visibility.

## Open Questions

- ? Which CICS program(s) use this BMS mapset (SEND/RECEIVE COPAU1A)?
  - Context: Not specified in the BMS source file.
- ? What are the exact data sources (e.g., VSAM files, DB2 tables) populating the map fields?
  - Context: BMS defines layout only, not data sources.
