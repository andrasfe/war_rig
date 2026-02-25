# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-02-25 15:29:03.102221

## Purpose

This BMS file defines the COPAU01 mapset with the COPAU1A map for displaying 'Pending Authorization Details Screen' in a CICS online application called CardDemo. The screen layout includes fields for transaction details such as card number, authorization date/time/response/code/amount/type/source, merchant details (name, ID, city, state, zip, MCC), fraud/match status, and error messaging. It supports input/output mode with auto storage, free cursor, and alarm control.

**Business Context**: Serves the CardDemo application for viewing pending credit card authorization details in a CICS transaction processing environment.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | CICS map for receiving and displaying authorization details data including card number (CARDNUM), auth date (AUTHDT), time (AUTHTM), response (AUTHRSP), merchant info (MERNAME, MERID, etc.), and status fields. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | CICS map for sending screen output with populated fields like transaction name (TRNNAME), program name (PGMNAME), current date/time (CURDATE, CURTIME), auth details, merchant details, fraud status (AUTHFRD), and error messages (ERRMSG). |

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

- ? Which specific CICS COBOL program(s) use this COPAU01 mapset?
  - Context: BMS file defines the map but does not reference calling programs.
- ? What are the transaction IDs or entry points that invoke this screen?
  - Context: Not specified in BMS definition; typically defined in CICS PCT.
