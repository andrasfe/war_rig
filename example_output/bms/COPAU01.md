# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-25 18:36:43.666529

## Purpose

BMS mapset COPAU01 defines the COPAU1A map for a CICS display-only screen titled 'Pending Authorization Details'. It presents transaction header (Tran, Date, Prog, Time), authorization details (Card #, Auth Date/Time, Resp, Reason, Code, Amount, POS Entry Mode, Source, MCC Code, Card Exp Date, Auth Type, Tran Id, Match/Fraud Status), and merchant details (Name, ID, City, State, Zip). Navigation aids include F3=Back, F5=Mark/Remove Fraud, F8=Next Auth, with error message area.

**Business Context**: Credit card authorization review screen in a fraud detection or pending auth workflow, displaying details for operator review.

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1A | IOType.CICS_MAP | Display-only CICS map populated by application program with authorization and merchant details for terminal output. |

## Open Questions

- ? What is the associated CICS COBOL program that uses this BMS map?
  - Context: BMS file does not reference calling program
- ? What transaction ID invokes this screen?
  - Context: Not specified in BMS source
- ? Default ATTRB behavior for unlabeled field at lines 229-233?
  - Context: No ATTRB specified; may default to transmittable on input despite INITIAL
