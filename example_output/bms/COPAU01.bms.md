# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-27 02:42:19.367437

## Purpose

This BMS mapset COPAU01 defines the screen layout named COPAU1A for the 'Pending Authorization Details Screen' in a CICS application. It displays transaction header, authorization details (card number, dates, times, responses, amounts, merchant info), and navigation options like F3=Back, F5=Mark/Remove Fraud, F8=Next Auth. The map uses colors, positions, and attributes to present read-only details with an error message area.

**Business Context**: Serves the CardDemo application for viewing and managing pending credit card authorization details, including fraud marking.

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | 24x80 character screen map displaying authorization details fields such as CARDNUM, AUTHDT, AUTHRSP, MERNAME, etc., with labels and initial values |

## Business Rules

- **BR001**: Screen triggers terminal alarm on errors or when transmitting modified unprotected fields
- **BR002**: Map operates in input/output mode with automatic storage and extended high-level attributes
- **BR003**: Function keys F3, F5, F8 provide navigation and action options: Back, Mark/Remove Fraud, Next Auth

## Open Questions

- ? Which fields are modifiable (unprotected) vs. protected output-only?
  - Context: ATTRB specifications like (ASKIP,NORM) do not explicitly state PROT/UNPROT; default behavior unclear without CICS BMS reference
- ? Exact transaction ID or CICS entry point using this map
  - Context: Not specified in BMS source
