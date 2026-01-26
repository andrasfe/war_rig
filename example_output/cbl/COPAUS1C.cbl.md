# COPAUS1C

**File**: `cbl/COPAUS1C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 15:12:29.184013

## Purpose

COPAUS1C is a CICS online transaction program that provides a detailed view of a selected pending authorization record from IMS database. It supports user interactions via BMS map including loading details on ENTER/PF keys, toggling fraud status on PF5, navigating to next record on PF8, and returning to summary view on PF3. Authorization details are read from IMS PAUTSUM0 root and PAUTDTL1 child segments, populated into output map COPAU1AO for display, with fraud updates via linked program.

**Business Context**: CardDemo Authorization Module - displays and manages details of pending authorization messages including fraud flagging for account review.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Communication area carrying account ID, selected PAU key, page info, fraud data from/to previous screens |
| COPAU1AI | IOType.CICS_MAP | Input BMS map fields from user screen interactions like selected auth key |
| PAUTSUM0 | IOType.IMS_SEGMENT | Pending Authorization Summary root segment qualified by account ID |
| PAUTDTL1 | IOType.IMS_SEGMENT | Pending Authorization Details child segment qualified by auth key |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output BMS map populated with auth details, header, error messages for screen display |
| PAUTDTL1 | IOType.IMS_SEGMENT | Updated Pending Authorization Details segment with toggled fraud status |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Updated commarea returned to caller with current selected key and context |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS2C | CallType.CICS_LINK | Handles fraud report/ removal processing and returns update status |
| COPAUS0C | CallType.CICS_XCTL | Transfers control back to authorization summary screen |

## Business Rules

- **BR001**: Authorization response status display: '00' is APPROVED ('A' green), else DECLINED ('D' red)
- **BR002**: Fraud status toggle: If already confirmed, remove fraud flag; else report as fraud
- **BR003**: Decline reason lookup and display from static table
- **BR004**: Input validation for numeric account ID and non-blank selected key before IMS read

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point paragraph that orchestrates the entire program flow for the CPVD transaction. It initializes flags and messages, then checks EIBCALEN to distinguish cold start from reentry: on cold start (EIBCALEN=0), it initializes commarea and XCTLs back to summary screen COPAUS0C. On reentry, it loads commarea from DFHCOMMAREA, clears fraud data, and if first reenter sets reenter flag and performs PROCESS-ENTER-KEY to load details before sending screen. Otherwise, it receives the map, evaluates EIBAID to dispatch: ENTER/PF8/PF5 process specific actions then send screen; PF3 returns to summary; other keys treat as enter but show invalid key message. It consumes EIB fields (EIBCALEN, EIBAID), commarea, and map inputs, producing updated commarea and screen sends. Business logic handles transaction navigation and reentry states via CDEMO-PGM-REENTER flag. Errors are indirectly handled via subordinate paragraphs. After dispatch, it always EXEC CICS RETURN with TRANSID CPVD and updated commarea. Calls subordinate paragraphs based on AID and state.

### PROCESS-ENTER-KEY
This paragraph handles the default action for ENTER key or invalid AID, validating and loading the selected authorization record for display. It clears the input map COPAU1AO to low-values, checks if CDEMO-ACCT-ID is numeric and CDEMO-CPVD-PAU-SELECTED is valid (not spaces/low-values), moving them to WS-ACCT-ID/WS-AUTH-KEY if so, then performs READ-AUTH-RECORD to fetch IMS segments. If PSB scheduled, it unschedules and syncpoints. If validation fails, sets ERR-FLG-ON. Always performs POPULATE-AUTH-DETAILS to format data into output map regardless of error. Inputs consumed: commarea fields CDEMO-ACCT-ID, CDEMO-CPVD-PAU-SELECTED. Outputs produced: WS-*-fields, populated map fields, IMS PCB state. Business logic enforces numeric acct and non-blank key before DB access to prevent invalid reads. No direct error handling beyond flag set; relies on populate to skip on error. Calls READ-AUTH-RECORD for data fetch and POPULATE-AUTH-DETAILS for formatting.

### MARK-AUTH-FRAUD
This paragraph toggles the fraud status on PF5 keypress by reading the current auth record, flipping the flag, calling fraud program, and updating IMS if successful. It moves commarea acct/selected key to WS-*, performs READ-AUTH-RECORD, then if PA-FRAUD-CONFIRMED sets remove else sets report fraud flag. Prepares WS-FRAUD-DATA with record/cust/acct, LINKs to COPAUS2C. If link normal and update success, performs UPDATE-AUTH-DETAILS; else rolls back and sets message from fraud msg. Updates selected key from PA- and repopulates details. Inputs: commarea CDEMO-ACCT-ID/CPVD-PAU-SELECTED/CUST-ID, IMS details. Outputs: updated WS-FRAUD-DATA param, IMS segment via update, message. Business logic implements toggle: confirmed->remove, else->report. Error handling: rollback on link error or fraud update fail, set err flag indirectly. Calls READ-AUTH-RECORD, LINK COPAUS2C, UPDATE-AUTH-DETAILS if success, ROLL-BACK/POPULATE on error/success.

### PROCESS-PF8-KEY
Handles PF8 next record navigation by reading current record then GNP next child segment, updating selected key if not EOF. Moves commarea acct/selected to WS-*, READ-AUTH-RECORD (fetches parent+child), then READ-NEXT-AUTH-RECORD for next details. If PSB sched, unsched+syncpoint. If AUTHS-EOF, sets no-erase, message 'last auth'; else moves PA-AUTH-KEY to selected and populates. Inputs: commarea keys, current IMS position. Outputs: next PENDING-AUTH-DETAILS, updated selected key in commarea, message on EOF. Business logic checks EOF flag post-GNP to detect end of siblings. Error handling via read paras. Calls READ-AUTH-RECORD, READ-NEXT-AUTH-RECORD, POPULATE-AUTH-DETAILS, TAKE-SYNCPOINT.

### POPULATE-AUTH-DETAILS
Formats and moves IMS auth details fields into output map COPAU1AO fields for screen display, skipping if ERR-FLG-ON. Consumes PENDING-AUTH-DETAILS fields like PA-CARD-NUM, dates, amounts, codes, fraud status. Produces map fields: CARDNUMO, AUTHDTO/AUTHTMO (reformatted), AUTHAMTO, AUTHRSPO/RSNO (status/reason lookup/color), AUTHCDO etc., merchant info, fraud date if flagged. Business logic: conditional status A/D with color, SEARCH decline table for desc, date/time/expiry reformats, fraud display if confirmed/removed. No direct reads/writes/ errors; validation via ERR-FLG-OFF. No calls.

### RETURN-TO-PREV-SCREEN
Prepares commarea for return to authorization summary screen COPAUS0C via XCTL. Sets CDEMO-FROM-*-fields to current tranid/program/context, CDEMO-PGM-ENTER true, then XCTL PROGRAM(CDEMO-TO-PROGRAM=COPAUS0C) with commarea. Inputs: WS-PGM-AUTH-SMRY, WS-CICS-TRANID. Outputs: modified commarea. Business logic: standard CICS navigation backtrack. No error handling. No subordinate calls.

### SEND-AUTHVIEW-SCREEN
Sends the populated COPAU1AO map to terminal with cursor, erasing if SEND-ERASE-YES. First performs POPULATE-HEADER-INFO for titles/date/time/tranid/program. Moves WS-MESSAGE to map error field, -1 to CARDNUML (attr?). Conditional ERASE on flag. Inputs: populated map, WS-MESSAGE, flags. Outputs: screen display. Business logic: erase on initial loads. No errors. Calls POPULATE-HEADER-INFO.

### RECEIVE-AUTHVIEW-SCREEN
Receives user input from COPAU1A map into COPAU1AI with NOHANDLE. Simple CICS RECEIVE. Inputs: terminal screen. Outputs: COPAU1AI fields. No logic/conditions/errors/calls.

### POPULATE-HEADER-INFO
Populates common header fields in COPAU1AO: titles from copy, tranid/program names, current date/time reformatted. Calls FUNCTION CURRENT-DATE to WS-CURDATE-DATA, formats MM/DD/YY and HH:MM:SS. Inputs: WS-*, copies. Outputs: TITLE01O/02O, TRNNAMEO etc. No conditions/errors/calls.

### READ-AUTH-RECORD
Schedules PSB if needed, then GU root PAUTSUM0 by acct-id SSA, if OK GNP child PAUTDTL1 by auth-key. Sets EOF flags on status, error on other: builds WS-MESSAGE with IMS code and sends screen. Inputs: WS-ACCT-ID/AUTH-KEY to SSAs. Outputs: PENDING-AUTH-*-segments, flags, message on err. Business logic: qualified reads parent->child. Errors: send screen+msg, no abend. Calls SCHEDULE-PSB, SEND-AUTHVIEW-SCREEN on err.

### READ-NEXT-AUTH-RECORD
Performs unqualified GNP next PAUTDTL1 sibling from current PCB position, sets flags on status, error msg+send screen. Inputs: current PCB. Outputs: next PENDING-AUTH-DETAILS, flags/message. Business logic: sequential next for PF8. Errors: send screen.

### UPDATE-AUTH-DETAILS
REPL current PAUTDTL1 with WS-FRAUD-AUTH-RECORD (updated fraud), syncpoint if OK with success msg based on remove/report; else rollback, err msg+send screen. Inputs: WS-FRAUD-*. Outputs: updated IMS segment, message. Business logic: post-fraud toggle update. Errors: rollback+screen.

### TAKE-SYNCPOINT
Exec CICS SYNCPOINT to commit changes. No inputs/outputs/logic.

### ROLL-BACK
Exec CICS SYNCPOINT ROLLBACK to undo DB changes. No inputs/outputs/logic.

### SCHEDULE-PSB
Exec DLI SCHD PSBPAUTB NODHABEND, term+resched if already scheduled, set flag on OK else err msg+send screen. Inputs: PSB-NAME. Outputs: PCB scheduled, flag/message. Business logic: handle TC status retry.

## Open Questions

- ? Exact field layouts and values in copybooks like CIPAUSMY, CIPAUDTY, COPAU01
  - Context: Source shows usage but not full definitions; inferred from moves like PA-CARD-NUM
- ? How COPAUS0C invokes this (exact XCTL or LINK?)
  - Context: Inferred from context/WS-PGM-AUTH-SMRY but not explicit call site
