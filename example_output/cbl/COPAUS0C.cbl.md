# COPAUS0C

**File**: `cbl/COPAUS0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 17:38:36.029640

## Purpose

COPAUS0C is a CICS COBOL program using IMS DL/I and BMS that provides a summary view of pending authorization messages for a given account ID in the CardDemo authorization module. It retrieves account, customer, and card cross-reference details from VSAM files, fetches authorization summary and detail segments from IMS database PAUTSUM0 and PAUTDTL1, and displays paginated lists (5 per page) with navigation via PF7 (previous) and PF8 (next). Users can select an authorization with 'S' on Enter to XCTL to detail program COPAUS1C, or PF3 to return to menu.

**Business Context**: CardDemo - Authorization Module for displaying summary views of authorization messages

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Common commarea containing account ID, program context, authorization keys for pagination and selection |
| COPAU0AI | IOType.CICS_MAP | BMS input map fields for account ID entry (ACCTIDI), selection fields (SEL0001I-SEL0005I), transaction details |
| CXACAIX | IOType.FILE_VSAM | Card XREF alternate index by account ID to get customer ID and card number |
| ACCTDAT | IOType.FILE_VSAM | Account master file read by account ID for credit limits and other account data |
| CUSTDAT | IOType.FILE_VSAM | Customer master file read by customer ID for name, address, phone |
| PAUTSUM0 | IOType.IMS_SEGMENT | IMS root segment for pending auth summary containing counts, balances by account |
| PAUTDTL1 | IOType.IMS_SEGMENT | IMS child segment for pending auth details containing individual authorizations |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | BMS output map populated with account details, customer info, auth summary counts/balances, paginated auth list |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Updated commarea with auth keys, page info, selection for return or XCTL |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS1C | CallType.CICS_XCTL | Transfer control to authorization detail view when user selects 'S' on a summary line |
| COMEN01C | CallType.CICS_XCTL | Return to menu screen on PF3 |
| COSGN00C | CallType.CICS_XCTL | Fallback return to signon screen if no previous program specified |

## Business Rules

- **BR001**: Account ID must be numeric and not blank/low-values for processing
- **BR002**: Selection field (SELxxxxI) must be 'S' or 's' to XCTL to details, else invalid
- **BR003**: Approved status 'A' if PA-AUTH-RESP-CODE = '00', else 'D' declined
- **BR004**: Display up to 5 auth details per page, track prev/last keys for pagination

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph that initializes flags, handles first entry vs reentry via commarea, receives/sends BMS map, and dispatches to key handlers based on EIBAID. It consumes EIBCALEN, DFHCOMMAREA for context, map input COPAU0AI on reentry, and produces updated map COPAU0AO, commarea, and final CICS RETURN with TRANSID 'CPVS'. Business logic checks if first call (EIBCALEN=0) to init screen, else processes commarea for reenter, gathers details if new acct, or evaluates AID: ENTER calls PROCESS-ENTER-KEY for selection/validation/XCTL, PF3 returns to menu, PF7/PF8 for pagination, other sets invalid key error. Error handling via WS-ERR-FLG and messages in WS-MESSAGE displayed on map. It calls GATHER-DETAILS, SEND-PAULST-SCREEN, RECEIVE-PAULST-SCREEN, and various PF handlers indirectly. Always ends with CICS RETURN preserving state for transaction restart.

### PROCESS-ENTER-KEY
Handles ENTER key press to validate and process account ID entry and line selections for detail view. Consumes map input ACCTIDI and SEL0001I-SEL0005I fields. Produces updated CDEMO-CPVS-PAU-SEL-FLG/SELECTED in commarea, WS-ACCT-ID, and error messages if invalid. Business logic: validates ACCTIDI not blank and numeric, maps SELxxxxI to index 1-5 for auth key selection, requires 'S'/'s' to XCTL COPAUS1C with selected key, else invalid message. Errors: non-numeric/blank acct sets WS-ERR-FLG 'Y' and cursor to ACCTIDL=-1. Calls GATHER-DETAILS to refresh list post-processing. No file I/O here, delegates to gather.

### GATHER-DETAILS
Orchestrates data retrieval and pagination setup for the current WS-ACCT-ID. Consumes WS-ACCT-ID from map/commarea. Produces populated map fields via subordinate calls, initializes page num to 0, clears ACCTIDL. Business logic: if valid acct, calls GATHER-ACCOUNT-DETAILS for VSAM data and summary counts, INITIALIZE-AUTH-DATA to blank list, then PROCESS-PAGE-FORWARD if summary segment found. No direct errors, propagates from children via flags. Role is to refresh display data on entry changes or pagination.

### PROCESS-PF7-KEY
Handles PF7 for previous page navigation. Consumes CDEMO-CPVS-PAGE-NUM and PAUKEY-PREV-PG array from commarea. Produces updated page num, WS-AUTH-KEY-SAVE from prev key, refreshed list. Business logic: if page >1, decrement page, load prev key, GET-AUTH-SUMMARY to reposition, set ERASE-NO and NEXT-PAGE-YES, init data and forward; else top-of-list message. Errors delegated. Calls chain to populate prior page.

### PROCESS-PF8-KEY
Handles PF8 for next page from last key. Consumes CDEMO-CPVS-PAUKEY-LAST. Produces reposition via REPOSITION-AUTHORIZATIONS if last key valid, sets NEXT-PAGE-YES/NO, ERASE-NO. Business logic: load last key, get summary, reposition details, if success set next yes and forward process, else bottom-of-list message. Enables forward pagination beyond current 5.

### PROCESS-PAGE-FORWARD
Fills the 5-line auth list for current page by sequential GNP calls. Consumes WS-IDX init 1, current IMS position. Produces CDEMO-CPVS-AUTH-KEYS(1-5), map fields TRNIDxxI/PDATE etc via POPULATE, tracks LAST and PREV-PG keys, peeks 6th for NEXT-PAGE-YES. Business logic: loop until 5 or EOF/ERR, reposition on PF7 first, else GNP next, populate if ok increment idx; after loop GNP one more for next flag. Errors stop loop. Central to pagination display.

### GET-AUTHORIZATIONS
Performs IMS GNP to fetch next PAUTDTL1 detail segment unconditionally. Consumes current PCB position. Produces PENDING-AUTH-DETAILS populated. Business logic: DLI GNP PAUTDTL1 INTO details, eval DIBSTAT: OK not-EOF, GE/GB EOF, other ERR with message and send screen. Advances sequential read for list.

### REPOSITION-AUTHORIZATIONS
Repositions IMS to specific PAUTDTL1 via key for pagination start. Consumes WS-AUTH-KEY-SAVE into PA-AUTHORIZATION-KEY. Produces PENDING-AUTH-DETAILS at key. Business logic: DLI GNP WHERE PAUT9CTS=KEY, eval STATUS-OK not-EOF, GE/GB EOF, other ERR message send screen.

### POPULATE-AUTH-LIST
Formats single auth detail into map line and commarea key by WS-IDX. Consumes PENDING-AUTH-DETAILS fields. Produces map TRNIDxxI, PDATE etc, PAPRVxxI, CDEMO-CPVS-AUTH-KEYS(idx), sets UNPROT. Business logic: move/format amt/date/time/status, eval idx 1-5 to specific fields. No conditions, always populates current.

### INITIALIZE-AUTH-DATA
Clears the 5-line auth list on map to protected blanks before populate. Consumes WS-IDX 1-5 loop. Produces map fields SPACES, SELxxxxA=PRO. Business logic: varying idx set all TRNIDxxI/PDATE etc spaces, PROT. Preps screen for new data.

### SEND-PAULST-SCREEN
Sends the BMS map COPAU0A with header, message, cursor. Consumes fully populated COPAU0AO, SEND-ERASE-FLG. Produces screen display ERASE if YES else accum. Business logic: if PSB schd syncpoint/unschd, populate header date/time/titles, move message, send MAPSET COPAU00 FROM AO CURSOR [ERASE]. Critical for all UI updates.

### RECEIVE-PAULST-SCREEN
Receives BMS map input into COPAU0AI with RESP codes. Consumes screen user input. Produces COPAU0AI fields populated. Business logic: CICS RECEIVE MAP COPAU0A INTO AI RESP/RESP2. Simple I/O.

### GATHER-ACCOUNT-DETAILS
Retrieves and displays static account/customer/summary data header. Consumes WS-ACCT-ID. Produces map CNAMEO, ADDR001O, CREDLIMO, APPRCNTO etc from VSAM/IMS. Business logic: chain GETCARDXREF_BYACCT -> GETACCTDATA_BYACCT -> GETCUSTDATA_BYCUST for records, format strings name/addr, move limits, GET-AUTH-SUMMARY for counts/bals if found else zero. No loop, one-time per acct.

### GET-AUTH-SUMMARY
Qualifies and gets IMS PAUTSUM0 root by acct ID. Consumes CDEMO-ACCT-ID to PA-ACCT-ID. Produces PENDING-AUTH-SUMMARY if found sets FOUND-PAUT-SMRY-SEG. Business logic: SCHEDULE-PSB, DLI GU WHERE ACCNTID=, eval OK found, GE notfound, other ERR send screen.

### SCHEDULE-PSB
Schedules IMS PSB 'PSBPAUTB' handling duplicates. No inputs. Sets IMS-PSB-SCHD flag or ERR. Business logic: DLI SCHD NODHABEND, if TC TERM+SCHD again, OK set Y else ERR message send.

## Open Questions

- ? Exact field layouts in copybooks like COPAU00, CIPAUSMY
  - Context: COPY statements provided but definitions not in source
- ? Called_by programs
  - Context: No self-references or JCL, unknown callers
- ? IMS database full structure/hierarchy
  - Context: PAUTSUM0 root, PAUTDTL1 child assumed from GNP after GU
