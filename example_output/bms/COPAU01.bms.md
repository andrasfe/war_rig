# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-26 14:23:23.186977

## Purpose

This BMS file defines the mapset COPAU01 containing the single map COPAU1A for the 'Pending Authorization Details Screen' in a CICS CardDemo application. It specifies screen layout including header fields for transaction ID, date, time, program name, titles, and detailed fields for card number, auth date/time/response/reason/code/amount/type/source, merchant details, and fraud/match status. The map supports display-only fields with labels, colors, positions, and bottom function keys for user actions.

**Business Context**: Review and management of pending card authorization details in a payment processing system

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | Symbolic map for receiving user input such as function key presses from the screen |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | Physical and symbolic map for sending authorization details to the 3270 screen display |

## Business Rules

- **BR001**: Screen navigation and actions controlled by function keys: F3=Back to previous screen, F5=Mark/Remove Fraud on the authorization, F8=Next Auth to view subsequent authorization

## Paragraphs/Procedures

### COPAU1A
This map definition serves as the primary screen layout for displaying pending authorization details in the CICS online transaction. It consumes data from the calling program's working storage variables mapped to symbolic fields such as CARDNUM (line 85), AUTHDT (94), AUTHTM (104), AUTHAMT (144), MERNAME (239), and others, which are populated before SEND MAP. It produces a formatted 24x80 screen output with protected label fields (ASKIP) in turquoise/blue/yellow and data fields in pink/blue/red/neutral colors for readability and highlighting (e.g., fraud status in red at 224). No business decisions are made here as it is a static layout definition; all fields are display-only (ATTRB includes ASKIP on data fields) except implied function key detection via RECEIVE MAP. Error handling is supported via the ERRMSG field (line 284) with attributes ASKIP,BRT,FSET for bright protected error message display. No subordinate paragraphs or programs are called as this is a BMS map, not executable code; it is referenced by CICS EXEC CICS SEND/RECEIVE MAP commands in the driving program.
