# COPAUS1C

**File**: `cbl/COPAUS1C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 02:30:48.987288

## Purpose

COPAUS1C is a CICS/IMS/BMS online program that provides a detail view of pending authorization messages for the CardDemo application. It retrieves and displays authorization details from IMS database segments, supports user interactions like selecting records, navigating to next records (PF8), toggling fraud status (PF5), and returning to summary screen (PF3). It handles screen I/O via BMS maps and manages IMS PSB scheduling, database reads/updates, and transaction linkage.

**Business Context**: Card authorization module in CardDemo application, allowing operators to view, navigate, and manage fraud flags on pending authorizations.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Program communication area containing account ID, selected authorization key, page info, and fraud data from previous screens |
| COPAU1AI | IOType.CICS_MAP | Input map from RECEIVE for user selections like selected authorization key |
| PENDING-AUTH-SUMMARY | IOType.IMS_SEGMENT | Root IMS segment for authorization summary, qualified by account ID |
| PENDING-AUTH-DETAILS | IOType.IMS_SEGMENT | Child IMS segment for authorization details, qualified by authorization key |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output map populated with authorization details, header info, and messages for screen display |
| PENDING-AUTH-DETAILS | IOType.IMS_SEGMENT | Updated IMS details segment with fraud status changes via REPL |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Updated commarea passed back on RETURN or XCTL |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS2C | CallType.CICS_LINK | To process fraud reporting or removal on the authorization record |
| COPAUS0C | CallType.CICS_XCTL | Return to authorization summary screen |

## Business Rules

- **BR001**: Display approval/decline status: 'A' for response code '00' with green color, 'D' otherwise with red color
- **BR002**: Lookup decline reason from table and format into AUTHRSNO
- **BR003**: Toggle fraud status: if confirmed, remove; else report as fraud

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph that initializes flags, handles first-time entry versus re-entry, processes commarea, and dispatches to appropriate actions based on EIBAID. It consumes DFHCOMMAREA on re-entry and map input via RECEIVE-AUTHVIEW-SCREEN, producing updated commarea and screen sends via SEND-AUTHVIEW-SCREEN. Business logic evaluates EIBAID for PF3 (return to summary), PF5 (mark fraud), PF8 (next record), ENTER (process selection), or invalid key handling with error message. Error handling sets WS-ERR-FLG and displays messages for invalid inputs. It calls PROCESS-ENTER-KEY for selections, MARK-AUTH-FRAUD for PF5, PROCESS-PF8-KEY for navigation, RETURN-TO-PREV-SCREEN for PF3, and always ends with CICS RETURN preserving commarea and transaction ID. On initial entry (EIBCALEN=0), it XCTLs back to summary screen.

### PROCESS-ENTER-KEY
This paragraph processes user selection of an authorization record via ENTER key. It consumes CDEMO-ACCT-ID and CDEMO-CPVD-PAU-SELECTED from commarea/map, validates numeric and non-spaces, reads IMS record via READ-AUTH-RECORD if valid, sets error flag otherwise. It produces populated map fields via POPULATE-AUTH-DETAILS and commits PSB if scheduled. Business logic checks input validity before IMS access to prevent invalid reads. No explicit error handling beyond flag set; relies on caller for display. Calls READ-AUTH-RECORD to fetch summary/details and POPULATE-AUTH-DETAILS to format screen data.

### MARK-AUTH-FRAUD
Handles PF5 to toggle fraud status on selected authorization. Consumes CDEMO-ACCT-ID and CDEMO-CPVD-PAU-SELECTED to read IMS record via READ-AUTH-RECORD, determines toggle (confirm/remove) based on PA-FRAUD-CONFIRMED flag. Produces WS-FRAUD-DATA for LINK to fraud program, updates IMS via UPDATE-AUTH-DETAILS if success, sets messages, rolls back on failure. Business logic toggles fraud flags and calls external program for processing. Error handling checks EIBRESP and WS-FRD-UPDT-SUCCESS, performs ROLL-BACK and error message on failures. Calls READ-AUTH-RECORD, LINK to COPAUS2C, UPDATE-AUTH-DETAILS, POPULATE-AUTH-DETAILS.

### PROCESS-PF8-KEY
Processes PF8 for next authorization record navigation. Consumes current selection to read current via READ-AUTH-RECORD, then GNP for next details via READ-NEXT-AUTH-RECORD. Produces updated selection in commarea and repopulates screen if not EOF, else sets no-erase and EOF message. Business logic handles end-of-authorizations by flagging EOF and messaging. Error handling in subordinate reads displays errors and sends screen. Calls READ-AUTH-RECORD, READ-NEXT-AUTH-RECORD, POPULATE-AUTH-DETAILS, commits PSB.

### POPULATE-AUTH-DETAILS
Formats IMS data into BMS map fields for display, only if no error. Consumes PENDING-AUTH-DETAILS fields like PA-CARD-NUM, dates, amounts, codes. Produces filled map fields in COPAU1AO such as CARDNUMO, AUTHDTO, AUTHRSNO, etc. Business logic transforms dates/times, determines status color, searches decline table, formats fraud info if flagged. No error handling; conditional on ERR-FLG-OFF. No calls.

### RETURN-TO-PREV-SCREEN
Prepares and XCTLs to previous summary program COPAUS0C. Consumes program names from WS-VARIABLES, sets context flags in commarea. Produces updated commarea with from-transid/program. Business logic sets enter flag and clears context for clean return. No error handling (CICS XCTL). Calls none.

### SEND-AUTHVIEW-SCREEN
Sends the populated BMS map to terminal. Consumes COPAU1AO map data, WS-MESSAGE, SEND-ERASE-FLG. Produces screen display with ERASE if flagged. Business logic calls POPULATE-HEADER-INFO first, conditional ERASE. No error handling (CICS SEND CURSOR NOHANDLE implied). Calls POPULATE-HEADER-INFO.

### RECEIVE-AUTHVIEW-SCREEN
Receives map input from user into COPAU1AI. Consumes screen data via CICS RECEIVE. Produces filled input map. No business logic or error handling (NOHANDLE). No calls.

### POPULATE-HEADER-INFO
Fills screen header with titles, transaction/program names, current date/time. Consumes WS-VARIABLES, FUNCTION CURRENT-DATE. Produces TITLE01O, TRNNAMEO, CURDATEO, CURTIMEO in map. Business logic formats date/time from WS-CURDATE-DATA. No errors. No calls.

### READ-AUTH-RECORD
Reads IMS authorization summary (GU) then details (GNP) for specific acct/key. Consumes WS-ACCT-ID, WS-AUTH-KEY into segments. Produces filled PENDING-AUTH-SUMMARY/DETAILS or EOF/error flags/messages. Business logic qualifies by ACCNTID and PAUT9CTS, handles IMS statuses with error strings and screen send on failure. Error handling for non-OK statuses sets EOF or ERR-FLG and sends screen. Calls SCHEDULE-PSB.

### READ-NEXT-AUTH-RECORD
Performs GNP for next PAUTDTL1 segment under current parent. Consumes current PCB context. Produces next PENDING-AUTH-DETAILS or EOF/error. Business logic sets statuses based on DIBSTAT, error message and screen on failure. Error handling sends screen on non-OK.

### UPDATE-AUTH-DETAILS
Replaces IMS details segment with updated fraud data from WS-FRAUD-AUTH-RECORD. Consumes PENDING-AUTH-DETAILS. Produces updated segment, commits on OK with success message, rolls back on error with message/screen. Business logic sets fraud messages based on flag, handles IMS status.

### TAKE-SYNCPOINT
Commits CICS/IMS changes. No inputs/outputs beyond CICS exec. No logic/errors.

### ROLL-BACK
Rolls back CICS/IMS changes on error. No inputs/outputs. No logic.

### SCHEDULE-PSB
Schedules IMS PSB 'PSBPAUTB', handles re-schedule if TC status. Consumes PSB-NAME. Produces scheduled PCB or error message/screen. Business logic retries TERM/SCHD on duplicate schedule, sets flag on OK. Error handling sends screen.

## Open Questions

- ? Exact field layouts in copybooks like CIPAUSMY, CIPAUDTY, COPAU01
  - Context: Not provided in source; inferred from usage but precise offsets/types unknown
- ? Behavior of called COPAUS2C fraud program
  - Context: LINK parameters and return values inferred but not detailed
- ? Full calling sequence from COPAUS0C
  - Context: Inferred from WS-PGM-AUTH-SMRY and XCTL
