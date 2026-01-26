# COPAUS0C

**File**: `cbl/COPAUS0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 15:12:25.395280

## Purpose

COPAUS0C is a CICS online program that displays a paginated summary list of pending authorization details for a given account ID in the CardDemo application. It retrieves account, customer, and card cross-reference data from VSAM files, fetches authorization summary and detail segments from IMS database, populates a BMS screen with up to 5 authorization entries per page, handles user selections to XCTL to detail view, and supports PF7/PF8 paging.

**Business Context**: Serves the CardDemo authorization module by providing a summary view of pending authorizations (approved/declined) for credit card accounts, enabling users to browse, select, and drill down to details.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Common commarea containing account ID, program context flags, previous/next page auth keys, page number, and selection flags from prior screens |
| COPAU0AI | IOType.CICS_MAP | BMS input map fields for account ID entry (ACCTIDI), selection fields (SEL0001I-SEL0005I), from mapset COPAU00 |
| CXACAIX | IOType.FILE_VSAM | Card cross-reference alternate index by account ID to retrieve customer ID and card number |
| ACCTDAT | IOType.FILE_VSAM | Account master file read by account ID for credit limits and other account details |
| CUSTDAT | IOType.FILE_VSAM | Customer master file read by customer ID for name, address, phone |
| PAUTSUM0 | IOType.IMS_SEGMENT | IMS root segment for pending auth summary (counts, balances, totals) qualified by account ID |
| PAUTDTL1 | IOType.IMS_SEGMENT | IMS child segment for pending auth details (transaction ID, date, time, type, status, amount) under summary |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | BMS output map populated with header (titles, date/time, tran/program), account details (customer name, address, limits), auth summary counts/balances, list of 5 auth details, error messages |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Updated commarea with selected auth key, page info, flags for XCTL/RETURN |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS1C | CallType.CICS_XCTL | Transfer control to authorization details screen with selected auth key |
| COMEN01C | CallType.CICS_XCTL | Return to menu screen on PF3 |
| COSGN00C | CallType.CICS_XCTL | Fallback to signon screen if no previous program set |

## Business Rules

- **BR001**: Account ID must be numeric and non-blank to retrieve details; otherwise display error message
- **BR002**: Selection field (SELxxxxI) must be 'S' or 's' to XCTL to details; invalid sets error
- **BR003**: Display up to 5 auth details per page; PF7 previous page if page >1 using saved prev key; PF8 next using last key
- **BR004**: Approval status 'A' if resp code '00', else 'D'; populate screen fields accordingly

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph that initializes flags, handles first-time entry vs reentry via commarea, receives map if applicable, dispatches to key handlers (ENTER/PF3/PF7/PF8), gathers details, sends the populated summary screen, and returns with transaction ID preserving commarea. It consumes EIBCALEN to detect initial call (0), DFHCOMMAREA for context, and BMS input map COPAU0AI for user data. It produces updated COPAU0AO map output, refreshed auth list and header info, and updated CARDDEMO-COMMAREA with page/selection data. Business logic includes checking reenter flag to decide init vs process, evaluating EIBAID for PF keys to invoke specific handlers like PROCESS-ENTER-KEY for selection/validation/XCTL or paging. Error handling sets WS-ERR-FLG and WS-MESSAGE for invalid keys, sending screen immediately. It calls GATHER-DETAILS to fetch/populate data, RECEIVE-PAULST-SCREEN, SEND-PAULST-SCREEN, and handlers like PROCESS-PF7-KEY/PF8-KEY for navigation, RETURN-TO-PREV-SCREEN for PF3. On initial call, initializes commarea and sends empty screen prompting for acct ID.

### PROCESS-ENTER-KEY
Handles ENTER keypress to validate/process account ID input and selection fields for drill-down to auth details. Consumes ACCTIDI from COPAU0AI and SEL000xI fields to determine selected auth key index. Produces updated CDEMO-CPVS-PAU-SEL-FLG/SELECTED in commarea, WS-ACCT-ID, error message if invalid, and XCTL to details if valid 'S'. Logic validates acct ID non-blank/numeric, maps SEL field to CDEMO-CPVS-AUTH-KEYS(n), checks 'S'/'s' for XCTL COPAUS1C with commarea. Errors for blank/non-numeric acct or invalid sel set WS-ERR-FLG/'Y', cursor to ACCTIDL, message. Calls GATHER-DETAILS to refresh list post-process.

### GATHER-DETAILS
Coordinates retrieval of account-related data and auth list population for the screen. Consumes WS-ACCT-ID to drive file reads and IMS GU for summary. Produces populated screen fields via sub-calls, initializes page num/keys, sets flags for paging. Logic checks acct ID valid, calls GATHER-ACCOUNT-DETAILS for VSAM data, INITIALIZE-AUTH-DATA to clear list, PROCESS-PAGE-FORWARD if summary found. No direct error handling, propagates from subs. Calls GATHER-ACCOUNT-DETAILS, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PF7-KEY
Handles backward paging (previous page) if not on page 1, repositions using saved prev key. Consumes CDEMO-CPVS-PAGE-NUM and PAUKEY-PREV-PG array. Produces decremented page num, sets WS-AUTH-KEY-SAVE to prev key, refreshes list via forward process. Logic checks page >1, computes page-1, moves prev key( page), GET-AUTH-SUMMARY, INITIALIZE/PROCESS-PAGE-FORWARD with REPOSITION if PF7. Error/message if already top. Calls GET-AUTH-SUMMARY, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PF8-KEY
Handles forward paging (next page) from last key, repositions to end. Consumes CDEMO-CPVS-PAUKEY-LAST. Produces NEXT-PAGE-YES/NO flag, refreshed list. Logic moves last key to save, GET-AUTH-SUMMARY, REPOSITION-AUTHORIZATIONS to position after last, then forward. Message if already bottom. Calls GET-AUTH-SUMMARY, REPOSITION-AUTHORIZATIONS, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PAGE-FORWARD
Fetches and populates up to 5 sequential auth detail segments for current page display. Consumes WS-IDX=1 init, current position from prior GNP/GU. Produces screen list fields TRNIDxxI/PDATExxI etc via POPULATE, saves LAST/PREV-PG keys, sets NEXT-PAGE-YES if more after 5. Loop PERFORM UNTIL idx>5/EOF/ERR: REPOSITION if PF7 idx1 else GET-AUTHORIZATIONS, POPULATE if ok, inc idx/save keys. After loop, peeks one more for next flag. Calls GET-AUTHORIZATIONS, REPOSITION-AUTHORIZATIONS, POPULATE-AUTH-LIST.

### GET-AUTHORIZATIONS
Performs IMS GNP call to retrieve next PAUTDTL1 child segment under current parent. Consumes current PCB position. Produces PENDING-AUTH-DETAILS populated, sets AUTHS-EOF if GE/GB. Logic GNP PAUTDTL1 INTO details, eval DIBSTAT: OK=not eof, GE/GB=eof, other=err msg/abend-like send. Calls SEND-PAULST-SCREEN on error.

### REPOSITION-AUTHORIZATIONS
Repositions IMS cursor to specific auth key via qualified GNP for paging. Consumes WS-AUTH-KEY-SAVE moved to PA-AUTHORIZATION-KEY. Produces PENDING-AUTH-DETAILS at that key, sets eof if not found. Logic GNP PAUTDTL1 WHERE PAUT9CTS=KEY, eval STATUS-OK/not-eof, GE/GB=eof, other err.

### POPULATE-AUTH-LIST
Transforms current PENDING-AUTH-DETAILS into screen fields for specific list row (1-5). Consumes PA fields like AMT, TIME, DATE, RESP-CODE, saves KEY to commarea array. Produces COPAU0AI fields like TRNID01I, PAMT001I, sets UNP for sel attr. Logic formats time/date, sets A/D stat, EVALUATE WS-IDX to move to row n fields.

### INITIALIZE-AUTH-DATA
Clears the 5 auth list fields on screen map to protected blanks before repopulate. Consumes WS-IDX 1-5 loop. Produces SPACES in TRNIDxxI/PDATE etc, PRO for sel attr. VARYING loop EVALUATE idx move SPACES/DFHBMPRO to row fields.

### SEND-PAULST-SCREEN
Sends the BMS map COPAU0A with cursor/erase if flagged, after IMS syncpoint if PSB scheduled. Consumes fully populated COPAU0AO, WS-MESSAGE to ERRMSGO. Produces screen display to user. Logic unschedule PSB/SYNCPOINT if sched, POPULATE-HEADER-INFO, move msg, SEND MAPSET FROM AO ERASE if yes else no.

### RECEIVE-PAULST-SCREEN
Receives user input from BMS map into COPAU0AI with resp codes. Consumes screen data. Produces COPAU0AI fields populated, WS-RESP-CD/REAS-CD for validation.

### POPULATE-HEADER-INFO
Fills static/dynamic header fields like titles, current date/time, tran/program names. Consumes FUNCTION CURRENT-DATE. Produces TITLE01O etc, formatted CURDATEO/CURTIMEO. Moves constants WS vars to output fields.

### GATHER-ACCOUNT-DETAILS
Retrieves and displays acct/cust summary info and auth totals from VSAM/IMS. Consumes WS-ACCT-ID driving chain. Produces screen fields CNAMEO, CREDLIMO, APPRCNTO etc from records. Calls VSAM reads xref/acct/cust, formats name/addr, moves limits, GET-AUTH-SUMMARY for totals if found else zeros.

### GETCARDXREF-BYACCT
Reads VSAM card xref AIX by acct ID to get cust/card nums. Consumes WS-ACCT-ID to RID. Produces CARD-XREF-RECORD, CDEMO-CUST-ID/NUM. Eval RESP: NORMAL ok, NOTFND/OTHER err msg/send.

### GETACCTDATA-BYACCT
Reads VSAM acct file by acct ID from xref. Consumes XREF-ACCT-ID. Produces ACCOUNT-RECORD. Eval RESP normal cont, notfnd/other err.

### GETCUSTDATA-BYCUST
Reads VSAM cust file by cust ID from xref. Consumes XREF-CUST-ID. Produces CUSTOMER-RECORD. Eval RESP normal cont, notfnd/other err.

### GET-AUTH-SUMMARY
Schedules PSB, GU qualified IMS root PAUTSUM0 by acct ID. Consumes CDEMO-ACCT-ID to PA-ACCT-ID. Produces PENDING-AUTH-SUMMARY, sets FOUND/NFOUND flags. Eval STATUS-OK found, GE nfound, other err.

### SCHEDULE-PSB
Schedules IMS PSBPAUTB, reschedules if already once, sets flag. Handles TERM if TC. Produces IMS-PSB-SCHD 'Y'. Err if not OK.

### RETURN-TO-PREV-SCREEN
Prepares XCTL to previous program (menu or signon) on PF3. Sets CDEMO-TO/FROM-PROGRAM/TRANID, context 0. Consumes CDEMO-TO-PROGRAM fallback COSGN00C.
