# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-26 15:13:57.330348

## Purpose

The COPAU01 BMS mapset defines a CICS 3270 screen layout named COPAU1A for displaying pending credit card authorization details in the CardDemo application. It includes static labels, output-only data fields for transaction, authorization response, merchant information, and an error message area. Function keys allow navigation and actions like backing out, marking fraud, or viewing next authorization.

**Business Context**: Serves the business process of viewing and managing pending authorization details in a credit card transaction processing system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | CICS map used for RECEIVE to capture function key inputs (e.g., F3, F5, F8) since no modifiable fields exist |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | CICS map used for SEND to display authorization details, merchant info, timestamps, and error messages |

## Business Rules

- **BR001**: The screen is display-only; users cannot enter or modify data fields, only interact via function keys

## Open Questions

- ? Which CICS COBOL program(s) include and use this BMS mapset?
  - Context: BMS file does not reference calling programs; must be determined from including source code
- ? What are the exact symbolic map field names and layouts in the generated copybook?
  - Context: BMS defines physical map; symbolic map details inferred from field names but not explicitly coded here
