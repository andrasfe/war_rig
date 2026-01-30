# COPAU01 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** COPAU01
- **File Name:** COPAU01.bms
- **File Type:** BMS
- **Analyzed By:** Scribe
- **Analyzed At:** 2026-01-30T19:45:36.664528

## Purpose

**Summary:** Defines BMS mapset COPAU01 with map COPAU1A for the 'Pending Authorization Details Screen' in CardDemo application. Displays authorization details including card number, auth date/time/response, amount, merchant info, fraud status, and supports function keys for navigation and actions like marking fraud. Used in CICS online transactions for viewing/acting on pending credit card authorizations.

**Business Context:** Credit card processing demonstration (CardDemo), specifically for reviewing pending authorization details to detect/support fraud management.
**Program Type:** ONLINE_CICS

## Inputs

### COPAU1A

- **Type:** CICS_MAP
- **Description:** CICS map for receiving user input such as function key presses (F3=Back, F5=Mark/Remove Fraud, F8=Next Auth) and any modifiable fields

## Outputs

### COPAU1A

- **Type:** CICS_MAP
- **Description:** CICS map for sending screen data including transaction details, auth info (AUTHDT, AUTHTM, AUTHRSP, etc.), merchant details (MERNAME, MERID, etc.), and error messages

## Paragraphs

### COPAU01

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

### COPAU1A

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Error Handling

- **Application errors:** Display message in ERRMSG field (bright, reverse, set attribute)

## Dead Code

- **COPAU01** (map): Artifact 'COPAU01' (map) is never referenced by any other artifact in the dependency graph
- **COPAU1A** (screen): Screen/Map 'COPAU1A' is never sent to or received from by any program
