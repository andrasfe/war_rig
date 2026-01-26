# COPAUS1C

**File**: `cbl/COPAUS1C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 17:38:31.011392

## Purpose

COPAUS1C is a CICS online program that displays detailed views of pending authorization records from an IMS database for a specific account. It handles user interactions such as selecting an authorization for details, navigating to the next record with PF8, toggling fraud status via PF5, and returning to the summary screen with PF3. The program populates a BMS screen (COPAU1A) with authorization details, including formatted dates, amounts, decline reasons, and fraud indicators.

**Business Context**: Part of CardDemo authorization module; provides detail view of authorization messages for account review, fraud management, and navigation in a banking/card processing system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Common commarea containing account ID, selected PAU key, page info, fraud data from previous program; passed via DFHCOMMAREA |
| COPAU1AI | IOType.CICS_MAP | Input map data from BMS screen receive, including user-entered or selected authorization key and account ID |
| PAUT PCB | IOType.IMS_SEGMENT | IMS database segments: PAUTSUM0 (root summary) qualified by ACCNTID, PAUTDTL1 (child details) qualified by PAUT9CTS=AUTH-KEY or GNP next |
| DFHAID | IOType.PARAMETER | CICS attention key identifier (EIBAID) for PF keys and ENTER |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output BMS map populated with auth details, header, error messages, sent to screen COPAU1A in mapset COPAU01 |
| PENDING-AUTH-DETAILS | IOType.IMS_SEGMENT | Updated PAUTDTL1 segment after fraud toggle, replaced via DLI REPL |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Updated commarea returned to caller or next program with current selected key, context |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS0C | CallType.CICS_XCTL | Return control to authorization summary screen (previous program) |
| COPAUS2C | CallType.CICS_LINK | Process fraud report or removal on the authorization record |

## Business Rules

- **BR001**: Toggle fraud status on authorization: if already confirmed, remove fraud flag; else set fraud confirmed
- **BR002**: Display decline reason by looking up response reason code in table
- **BR003**: Validate input for numeric account ID and non-blank selected key before reading IMS
- **BR004**: Response status display: 'A' green for '00', else 'D' red

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph that initializes flags, handles first-time entry vs re-enter, processes commarea, and dispatches based on EIBAID (ENTER, PF3, PF5, PF8). It consumes DFHCOMMAREA/EIBCALEN, EIBAID, and map input if re-enter; produces updated commarea and sends screen or XCTL to previous. Business logic evaluates aid keys: ENTER/PF8 read/populate/send; PF5 fraud toggle; PF3 return to summary; invalid sets error message. Error handling via WS-ERR-FLG and WS-MESSAGE display. Calls subordinate paragraphs like PROCESS-ENTER-KEY, SEND-AUTHVIEW-SCREEN, RETURN-TO-PREV-SCREEN; ultimately CICS RETURN with TRANSID CPVD. Ensures screen erase on initial send.

### PROCESS-ENTER-KEY
Handles ENTER key processing by validating numeric ACCT-ID and non-blank PAU-SELECTED, then reading specific IMS auth record via key. Consumes CDEMO-ACCT-ID, CDEMO-CPVD-PAU-SELECTED from commarea/map; produces WS-ACCT-ID/KEY for IMS read, sets ERR-FLG if invalid. Logic checks NUMERIC and NOT SPACES/LOW-VALUES before GU read; unschedules PSB post-read. No direct writes but populates details for screen. Calls READ-AUTH-RECORD then POPULATE-AUTH-DETAILS; error sets ERR-FLG-ON without screen send here.

### MARK-AUTH-FRAUD
Processes PF5 to toggle fraud status: reads current auth record, toggles PA-FRAUD-CONFIRMED/REMOVED flags, calls fraud program via LINK, updates IMS if success else rollback. Consumes CDEMO-ACCT-ID/PAU-SELECTED, IMS details; produces WS-FRAUD-DATA for LINK, updated PENDING-AUTH-DETAILS. Business logic: if confirmed then remove else report; LINK COPAUS2C; if EIBRESP NORMAL and success then REPL else ROLLBACK/ERRMSG. Error handling: ROLL-BACK on LINK fail or fraud fail, sets WS-MESSAGE. Calls READ-AUTH-RECORD, UPDATE-AUTH-DETAILS, POPULATE-AUTH-DETAILS.

### PROCESS-PF8-KEY
Handles PF8 for next authorization: reads current summary/details, GNP next details segment, handles EOF with message. Consumes WS-ACCT-ID/KEY from commarea; produces next PA-AUTH-KEY in commarea/selected. Logic: read current then GNP next PAUTDTL1; if EOF set ERASE-NO and message else update selected and populate. Error handling via IMS status in read-next. Calls READ-AUTH-RECORD, READ-NEXT-AUTH-RECORD, POPULATE-AUTH-DETAILS; unschedules PSB.

### POPULATE-AUTH-DETAILS
Formats and moves IMS auth details to output map fields if no error; transforms dates/times, amounts, reasons, fraud info. Consumes PENDING-AUTH-DETAILS fields (PA-CARD-NUM etc.); produces COPAU1AO fields (CARDNUMO, AUTHDTO etc.). Business logic: conditional response color/status, SEARCH decline table, fraud date format if flagged. No errors handled here (pre-checked ERR-FLG-OFF). No calls.

### RETURN-TO-PREV-SCREEN
Prepares commarea for return to summary screen via XCTL: sets FROM-TRANID/PGM, context zeros, enter flag. Consumes WS-PGM-AUTH-SMRY; produces updated CARDDEMO-COMMAREA. No conditions or errors; direct XCTL. Called on initial or PF3.

### SEND-AUTHVIEW-SCREEN
Sends the populated BMS map to screen: calls header populate, moves message, conditional ERASE, always CURSOR. Consumes COPAU1AO populated fields, WS-MESSAGE; produces screen display. Logic: if SEND-ERASE-YES then ERASE else no. No error handling. Calls POPULATE-HEADER-INFO.

### RECEIVE-AUTHVIEW-SCREEN
Receives map input from screen into COPAU1AI; NOHANDLE so ignores errors. Consumes screen data; produces COPAU1AI for processing. Called on re-enter.

### POPULATE-HEADER-INFO
Populates screen header with titles, tranid, pgmname, current date/time formatted. Consumes WS-CICS-TRANID etc., FUNCTION CURRENT-DATE; produces TITLE01O etc. in COPAU1AO. No conditions.

### READ-AUTH-RECORD
Schedules PSB if needed, GU root PAUTSUM0 by ACCNTID, then GNP child PAUTDTL1 by KEY; handles IMS statuses with EOF/error messages/screen send. Consumes WS-ACCT-ID/KEY; produces PENDING-AUTH-SUMMARY/DETAILS, sets AUTHS-EOF/ERR-FLG. Logic: EVALUATE IMS-RETURN-CODE for OK/GE/GB/OTHER; error STRING message and SEND. Calls SCHEDULE-PSB, SEND-AUTHVIEW-SCREEN.

### READ-NEXT-AUTH-RECORD
GNP next PAUTDTL1 under current parent; sets EOF or error/message/screen. Consumes current PCB position; produces next PENDING-AUTH-DETAILS. EVALUATE IMS statuses similar to read.

### UPDATE-AUTH-DETAILS
REPL current PAUTDTL1 with fraud-updated record; SYNCPOINT if OK else ROLLBACK/error message/screen. Consumes WS-FRAUD-AUTH-RECORD; produces updated IMS segment, sets WS-MESSAGE success/fail. EVALUATE IMS status.

### TAKE-SYNCPOINT
Exec CICS SYNCPOINT to commit changes.

### ROLL-BACK
Exec CICS SYNCPOINT ROLLBACK to undo changes.

### SCHEDULE-PSB
DLI SCHD PSBPAUTB, TERM/resched if TC, set schd flag or error/message/screen.

## Open Questions

- ? Exact field layouts in copybooks (e.g. full PA- fields in CIPAUSMY/CIPAUDTY)
  - Context: Copybooks not provided; inferred from usage like PA-ACCT-ID, PA-FRAUD-CONFIRMED
- ? Programs calling this one
  - Context: Inferred COPAUS0C from context/WS-PGM-AUTH-SMRY, but not explicitly stated
- ? Full fraud logic in COPAUS2C
  - Context: Only input/output flags visible (WS-FRD-UPDT-SUCCESS, WS-FRD-ACT-MSG)
