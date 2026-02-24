# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-02-24 03:59:25.688050

## Purpose

Defines the COPAU00 mapset and COPAU0A map for the Pending Authorization Screen in a CICS CardDemo application. The screen displays customer account details, credit/cash limits and balances, approval/decline counts, and a list of up to 5 pending transactions with selection options. Users can input an Account ID to search and select a transaction (S) for details.

**Business Context**: Credit card authorization pending review process, allowing tellers or operators to view and select pending authorizations by account.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID used to search for pending authorizations |
| SEL0001 | IOType.CICS_MAP | Selection field for first pending transaction |
| SEL0002 | IOType.CICS_MAP | Selection field for second pending transaction |
| SEL0003 | IOType.CICS_MAP | Selection field for third pending transaction |
| SEL0004 | IOType.CICS_MAP | Selection field for fourth pending transaction |
| SEL0005 | IOType.CICS_MAP | Selection field for fifth pending transaction |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | Full screen map displaying headers, customer info, limits, balances, counts, and transaction list |
| CNAME | IOType.CICS_MAP | Customer name display |
| CUSTID | IOType.CICS_MAP | Customer ID display |
| ERRMSG | IOType.CICS_MAP | Error message display area |

## Business Rules

- **BR001**: User input accepted only in unprotected fields (UNPROT attribute); all other fields are display-only with ASKIP attribute.
- **BR002**: Selection fields (SEL0001-SEL0005) allow user to choose transaction for details view by typing 'S'.

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
  - Context: BMS file defines map but does not reference calling programs.
- ? What are the exact data structures (copybooks) mapped to these fields in the using program?
  - Context: BMS defines fields but not underlying data records.
