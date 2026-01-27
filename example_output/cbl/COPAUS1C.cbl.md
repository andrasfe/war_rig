# COPAUS1C

**File**: `cbl/COPAUS1C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-27 02:41:03.853476

## Purpose

This CICS/IMS/BMS COBOL program provides a detail view screen for pending authorization records in the CardDemo authorization module. It reads IMS segments for authorization summary (PAUTSUM0) and details (PAUTDTL1), populates a BMS map with formatted details including dates, amounts, response codes, and fraud status, and handles user keys for enter (refresh details), PF3 (return to summary), PF5 (toggle fraud marking via linked program), and PF8 (next record). It supports fraud reporting/removal by linking to COPAUS2C and updating the IMS detail segment.

**Business Context**: Serves the authorization inquiry process in a simulated credit card demo application, allowing operators to view, navigate, and manage (mark/remove fraud) pending authorization details for accounts.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Common commarea containing account ID (CDEMO-ACCT-ID), selected authorization key (CDEMO-CPVD-PAU-SELECTED), customer ID (CDEMO-CUST-ID), page info, and fraud data passed from/to calling programs. |
| COPAU1AI | IOType.CICS_MAP | Input map from BMS receive containing user-entered or selected data like account ID and authorization key. |
| PAUTSUM0 | IOType.IMS_SEGMENT | IMS root segment (PENDING-AUTH-SUMMARY) for pending authorization summary, qualified by account ID. |
| PAUTDTL1 | IOType.IMS_SEGMENT | IMS child segment (PENDING-AUTH-DETAILS) for authorization details, qualified by authorization key under summary parent. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output BMS map populated with authorization details, header info, error messages, and formatted fields for screen display. |
| PAUTDTL1 | IOType.IMS_SEGMENT | Updated IMS child segment with fraud status changes after toggle. |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Updated commarea returned to caller or next transaction with current selected key and context. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS2C | CallType.CICS_LINK | Handles fraud marking or removal on the authorization detail record, updates fraud status, and returns success/failure message. |
| COPAUS0C | CallType.CICS_XCTL | Returns control to the authorization summary screen (previous program). |

## Business Rules

- **BR001**: Format authorization response: 'A' (Approved) with green color if PA-AUTH-RESP-CODE = '00', else 'D' (Declined) with red color.
- **BR002**: Lookup decline reason description from table by PA-AUTH-RESP-REASON code, append to code with '-' separator; default to '9999-ERROR' if not found.
- **BR003**: Toggle fraud status: If already PA-FRAUD-CONFIRMED, set to PA-FRAUD-REMOVED and call fraud program to remove; else set confirmed and report fraud.
- **BR004**: Validate enter key input: Require numeric CDEMO-ACCT-ID and non-blank/low-values CDEMO-CPVD-PAU-SELECTED before reading IMS.

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph that initializes flags and message areas, then handles first-time invocation (EIBCALEN=0) by XCTL back to summary screen or re-entry by moving commarea and dispatching based on reenter flag. For non-reenter, it processes enter key and sends screen; for reenter, receives map and evaluates EIBAID to handle PF3 (return to summary), PF5 (mark fraud), PF8 (next record), ENTER (refresh), or other (error). It consumes DFHCOMMAREA (via EIBCALEN), CARDDEMO-COMMAREA, and map input on receive. It produces updated CARDDEMO-COMMAREA and final CICS RETURN with TRANSID CPVD. Business logic includes conditional initialization, reenter detection (CDEMO-PGM-REENTER), and AID-based dispatching for navigation/error. Errors set WS-MESSAGE and redisplay screen. It calls subordinate paragraphs like PROCESS-ENTER-KEY, SEND-AUTHVIEW-SCREEN, RECEIVE-AUTHVIEW-SCREEN, MARK-AUTH-FRAUD, etc., and XCTL to COPAUS0C on PF3. Finally, always RETURNS to transaction CPVD preserving commarea.

### PROCESS-ENTER-KEY
This paragraph validates and refreshes the current authorization detail by reading IMS for the selected key. It consumes CDEMO-ACCT-ID (numeric check) and CDEMO-CPVD-PAU-SELECTED (non-blank/low check) from commarea/map, clears output map (LOW-VALUES to COPAU1AO), and performs READ-AUTH-RECORD using WS-ACCT-ID/WS-AUTH-KEY. If validation fails, sets ERR-FLG-ON. It syncpoints if PSB was scheduled. Produces populated map fields via subsequent POPULATE-AUTH-DETAILS. Business logic enforces numeric acct and valid selection before IMS GU read; errors flag only (no message here). No direct calls except READ-AUTH-RECORD (inline), then always calls POPULATE-AUTH-DETAILS to format display data. Error handling is validation-only; IMS errors handled in READ-AUTH-RECORD.

### MARK-AUTH-FRAUD
This paragraph toggles fraud status on the current authorization detail via PF5, first reading IMS record then conditionally setting confirmed/removed flags. It consumes CDEMO-ACCT-ID, CDEMO-CPVD-PAU-SELECTED to read IMS, toggles PA-FRAUD-CONFIRMED/PA-FRAUD-REMOVED and prepares WS-FRAUD-DATA (acct/cust ID, record, action). Links to COPAUS2C; on success and update success, updates IMS REPL and sets success message; else rolls back and sets error message from fraud program. Produces updated PENDING-AUTH-DETAILS in IMS, refreshed map, and updated CDEMO-CPVD-PAU-SELECTED. Business logic toggles fraud state (confirmed->remove else report), commits only on full success. Errors from LINK/REPL trigger ROLL-BACK. Calls READ-AUTH-RECORD, LINK COPAUS2C, UPDATE-AUTH-DETAILS/POPULATE-AUTH-DETAILS on success, ROLL-BACK on failure.

### PROCESS-PF8-KEY
Handles PF8 to navigate to next authorization detail record under the same account. Consumes current CDEMO-ACCT-ID/SELECTED to position via prior read, then GNP next PAUTDTL1 child. Syncpoints PSB if scheduled. If EOF, sets no-erase flag and 'last auth' message; else updates selected key and populates details. Produces next PENDING-AUTH-DETAILS or error message. Business logic detects AUTHS-EOF from IMS status to prevent wrap-around. Errors from GNP trigger message/screen send inline. Calls READ-AUTH-RECORD (to position?), READ-NEXT-AUTH-RECORD, POPULATE-AUTH-DETAILS.

### POPULATE-AUTH-DETAILS
Formats and moves IMS detail data to BMS output map fields only if no error flag, transforming dates/times/reasons/fraud info. Consumes PENDING-AUTH-DETAILS fields (PA-CARD-NUM, dates, amounts, codes, fraud flags) and decline table. Produces all screen fields like CARDNUMO, AUTHDTO (reformatted date), AUTHRSNO (lookup reason), AUTHFRDO (fraud date or '-'), merchant info etc. Business logic: conditional approved/decline display/color (BR001), table search for reason (BR002), fraud display if flagged, date/time reformats. No error handling (skips if ERR-FLG-ON). No calls.

### RETURN-TO-PREV-SCREEN
Prepares commarea for return to authorization summary screen on PF3 or initial call. Sets CDEMO-FROM-TRANID/PROGRAM to current, context zeros, enter true, then XCTL to COPAUS0C. Consumes nothing new. Produces updated CARDDEMO-COMMAREA for caller. Business logic: standard navigation back. No errors. No subordinate calls.

### SEND-AUTHVIEW-SCREEN
Sends the populated BMS map COPAU1A to terminal, with header population and conditional ERASE. Consumes populated COPAU1AO, WS-MESSAGE. Produces screen display, cursor positioned. Business logic: ERASE if SEND-ERASE-YES (initial), else refresh. Calls POPULATE-HEADER-INFO first. No errors (NOHANDLE assumed).

### RECEIVE-AUTHVIEW-SCREEN
Receives map input from terminal into COPAU1AI. Consumes screen data. Produces COPAU1AI fields for AID/evaluation. Business logic: none, just RECEIVE NOHANDLE. No errors handled here.

### POPULATE-HEADER-INFO
Populates static screen header with titles, transaction/program names, current date/time from FUNCTION CURRENT-DATE. Consumes WS-CICS-TRANID, WS-PGM-AUTH-DTL, date copybook vars. Produces TITLE01O etc., CURDATEO, CURTIMEO in map. Business logic: formats date/time HH:MM:SS, MM/DD/YY. No errors/calls.

### READ-AUTH-RECORD
Schedules PSB if needed, then GU reads summary root by acct ID, then GNP child details by auth key; sets EOF/error flags. Consumes WS-ACCT-ID/KEY to qualify. Produces PENDING-AUTH-SUMMARY/DETAILS or EOF/ERR-FLG. Business logic: IMS status eval (OK->not EOF, GE/GB->EOF, other->error msg/send screen). Errors string IMS-RETURN-CODE to WS-MESSAGE and send screen. Calls SCHEDULE-PSB.

### READ-NEXT-AUTH-RECORD
GNP next child detail under current parent positioning. Consumes current PCB position. Produces next PENDING-AUTH-DETAILS or EOF/error. Business logic: status OK->not EOF, GE/GB->EOF, other->error msg/screen. Errors set ERR-FLG and send.

### UPDATE-AUTH-DETAILS
REPL IMS detail segment with fraud-updated record from WS-FRAUD-AUTH-RECORD. On OK, syncpoint and set success msg (remove or mark); else rollback, err msg/screen. Consumes PENDING-AUTH-DETAILS (restored from fraud). Produces updated IMS segment. Business logic: conditional msg on PA-FRAUD-REMOVED. Calls TAKE-SYNCPOINT/ROLL-BACK.

### TAKE-SYNCPOINT
Performs CICS SYNCPOINT to commit changes. No inputs/outputs beyond CICS state. No logic/errors/calls.

### ROLL-BACK
Performs CICS SYNCPOINT ROLLBACK to undo DB changes. No inputs/outputs. No logic.

### SCHEDULE-PSB
Schedules IMS PSB 'PSBPAUTB' with retry if already scheduled (TERM then SCHD). Sets IMS-PSB-SCHD flag on OK, else err msg/screen. Consumes nothing. Produces PCB ready. Business logic: handle TC status. Errors string code and send screen.

## Open Questions

- ? Exact field layouts in copybooks like CIPAUSMY, CIPAUDTY, COPAU01
  - Context: COPY statements present but contents not in source; inferred from usage (e.g., PA-ACCT-ID, CARDNUMO)
- ? Full list of calling programs beyond COPAUS0C
  - Context: Inferred from WS-PGM-AUTH-SMRY='COPAUS0C' and context
- ? Details of fraud program COPAUS2C response in WS-FRD-ACT-MSG
  - Context: Used but not defined here
