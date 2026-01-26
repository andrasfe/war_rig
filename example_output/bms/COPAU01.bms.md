# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-26 17:40:52.520665

## Purpose

The COPAU01 BMS mapset defines the COPAU1A map for the 'Pending Authorization Details Screen' in a CICS online application. It layouts fields for displaying transaction header (Tran, Date, Prog, Time), authorization details (Card #, Auth Date/Time, Resp, Reason, Code, Amount, POS Entry Mode, Source, MCC Code, Card Exp Date, Auth Type, Tran Id, Match Status, Fraud Status), merchant details (Name, ID, City, State, Zip), error messages, and function keys (F3=Back, F5=Mark/Remove Fraud, F8=Next Auth). The map operates in INOUT mode for both display and user input on unprotected fields.

**Business Context**: Serves the CardDemo application for viewing and managing pending card authorization details, including fraud marking.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | Receives user input from unprotected modifiable fields such as CARDNUM (card number), AUTHMTC (match status), AUTHFRD (fraud status), and others without PROT attribute for actions like fraud marking. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | Displays authorization details, merchant information, timestamps, transaction data, error messages (ERRMSG), and static labels/function keys to the terminal. |

## Open Questions

- ? Specific transaction ID or CICS program that uses this mapset
  - Context: BMS file defines the map but does not reference calling programs or entry points.
