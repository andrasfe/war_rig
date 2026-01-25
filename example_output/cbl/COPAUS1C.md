# COPAUS1C

**File**: `cbl/COPAUS1C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-25 18:35:11.238505

## Purpose

CICS transaction CPVD displays detailed view of a selected pending authorization record from IMS database. Handles user interactions via BMS map COPAU1A including enter to load details, PF3 to return to summary, PF5 to toggle fraud status by linking to fraud program, PF8 to navigate next authorization. Updates authorization details with fraud flags upon successful fraud processing.

**Business Context**: CardDemo application authorization module for viewing and managing pending authorization details including fraud marking.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Contains account ID, selected authorization key (CDEMO-CPVD-PAU-SELECTED), customer ID, page info, and fraud data from previous program or reentry. |
| COPAU1AI | IOType.CICS_MAP | BMS map receive for screen input fields including AID keys (EIBAID), but map fields not used for authorization key selection. |
| PAUTSUM0 | IOType.IMS_SEGMENT | Pending authorization summary segment (root) qualified by account ID. |
| PAUTDTL1 | IOType.IMS_SEGMENT | Pending authorization details segment (child) qualified by authorization key. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | BMS map output populating authorization detail screen fields like card number, date, amount, response, merchant info, fraud status. |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Updated commarea returned to transaction CPVD or XCTL target. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS2C | CallType.CICS_LINK | Mark or remove fraud flag on authorization record and return status. |
| COPAUS0C | CallType.CICS_XCTL | Return to authorization summary screen. |

## Business Rules

- **BR001**: Display approved status in green if response code '00', else decline in red.
- **BR002**: Lookup decline reason description from table by response reason code.
- **BR003**: Toggle fraud status: if confirmed set to removed, else set to confirmed and report.
- **BR004**: Validate numeric account ID and non-blank selected auth key before reading IMS.

## Paragraphs/Procedures

### MAIN-PARA
Main entry point handling initial load or reentry, AID evaluation, screen send/receive, CICS return.

### PROCESS-ENTER-KEY
Validate and read selected auth record from IMS, populate details.

### MARK-AUTH-FRAUD
Toggle fraud flag, link to fraud program COPAUS2C, update details if success.

### PROCESS-PF8-KEY
Read current then next auth detail record, handle EOF.

### POPULATE-AUTH-DETAILS
Transform IMS details to BMS map fields including dates, status, lookup reasons, fraud.

### RETURN-TO-PREV-SCREEN
XCTL to previous summary program COPAUS0C.

### SEND-AUTHVIEW-SCREEN
Send BMS map with optional ERASE, cursor positioning.

### RECEIVE-AUTHVIEW-SCREEN
Receive BMS map into COPAU1AI.

### POPULATE-HEADER-INFO
Set screen header with titles, transaction, program, current date/time.

### READ-AUTH-RECORD
Schedule PSB, GU PAUTSUM0 by acct, GNP PAUTDTL1 by key, handle IMS statuses.

### READ-NEXT-AUTH-RECORD
GNP next PAUTDTL1, handle statuses.

### UPDATE-AUTH-DETAILS
REPL PAUTDTL1 with updated fraud data, commit or rollback.

### TAKE-SYNCPOINT
CICS SYNCPOINT commit.

### ROLL-BACK
CICS SYNCPOINT ROLLBACK.

### SCHEDULE-PSB
Schedule IMS PSBPAUTB, handle reschedule if needed, error to screen.

## Open Questions

- ? Exact fields in copybook COPAU01 for COPAU1AI/AO maps.
  - Context: Copybook contents not provided, only referenced.
- ? Full structure of CARDDEMO-COMMAREA beyond CDEMO-CPVD-INFO.
  - Context: Partially defined in COCOM01Y, but COPAU01 may extend.
- ? How CDEMO-PGM-REENTER is set/used across programs.
  - Context: Set to TRUE on first non-zero CALEN, but origin unclear.
