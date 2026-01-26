# COPAUS0C

**File**: `cbl/COPAUS0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 02:30:55.148775

## Purpose

This CICS/IMS/BMS COBOL program provides a paginated summary view of pending authorization details for a specified account in the CardDemo authorization module. It retrieves related account, customer, card cross-reference data from VSAM files and authorization summary/details from IMS database segments PAUTSUM0/PAUTDTL1. Users enter an account ID, navigate lists with PF7/PF8, select 'S' for details via XCTL to COPAUS1C, or PF3 to menu.

**Business Context**: Serves card authorization processing by displaying summary of pending auths including transaction ID, date, time, type, approval status, match status, and amount for account review.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DFHCOMMAREA | IOType.CICS_COMMAREA | CARDDEMO-COMMAREA from copybook COCOM01Y containing account ID, program context, auth keys, page info |
| COPAU0AI | IOType.CICS_MAP | Input map fields for account ID (ACCTIDI), selection fields (SEL0001I-SEL0005I) |
| CXACAIX | IOType.FILE_VSAM | Card XREF file accessed by ACCT-ID alternate index for customer/card linkage |
| ACCTDAT | IOType.FILE_VSAM | Account master file read by ACCT-ID |
| CUSTDAT | IOType.FILE_VSAM | Customer master file read by CUST-ID |
| PAUTSUM0 | IOType.IMS_SEGMENT | IMS root segment for pending auth summary by ACCT-ID |
| PAUTDTL1 | IOType.IMS_SEGMENT | IMS child segment for pending auth details, GU/GNP calls |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | Output map populated with account details, customer info, auth summary counts/balances, list of 5 auths |
| DFHCOMMAREA | IOType.CICS_COMMAREA | Updated CARDDEMO-COMMAREA with auth keys, page info, selection for return/XCTL |
| WS-MESSAGE | IOType.OTHER | Error/informational messages displayed on map |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS1C | CallType.CICS_XCTL | Transfer control to authorization details screen when 'S' selected on list entry |
| COMEN01C | CallType.CICS_XCTL | Return to menu screen via CDEMO-TO-PROGRAM on PF3 |
| COSGN00C | CallType.CICS_XCTL | Fallback to signon screen if no previous program specified |

## Business Rules

- **BR001**: Account ID must be numeric and non-blank for processing
- **BR002**: Selection field (SELxxxxI) must be 'S' or 's' to XCTL to details
- **BR003**: Approval status derived from PA-AUTH-RESP-CODE: '00'=A(Approved), else D(Declined)

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point orchestrating the entire program flow for the authorization summary screen. It initializes flags (ERR-FLG-OFF, AUTHS-NOT-EOF, SEND-ERASE-YES), clears messages and sets ACCTIDL to -1. On first entry (EIBCALEN=0), initializes commarea and sends initial screen. On reentry, receives map if not reenter mode, gathers details for account, or processes PF keys: ENTER for selection/validation/XCTL, PF3 to menu, PF7/PF8 for page navigation. Always ends with CICS RETURN TRANSID(CPVS) updating commarea. Error handling via WS-ERR-FLG and messages. Calls GATHER-DETAILS, SEND-PAULST-SCREEN, RECEIVE-PAULST-SCREEN, PROCESS-ENTER-KEY etc. as needed.

### PROCESS-ENTER-KEY
Handles ENTER key to validate/process account ID input and selection fields. Consumes ACCTIDI from map and SEL000xI fields, checks blank/low-values or non-numeric ACCTIDI setting error/message/clear field. If valid, stores ACCT-ID to WS/commarea, maps selection to CDEMO-CPVS-PAU-SELECTED auth key. If selection 'S'/'s', XCTL to COPAUS1C details with context. Else invalid selection message. Always calls GATHER-DETAILS to refresh list. Errors set WS-ERR-FLG and clear ACCTIDL.

### GATHER-DETAILS
Refreshes screen data for current WS-ACCT-ID by clearing ACCTIDL, resetting page num, gathering account/customer/xref details if valid ID, initializing auth list, and processing first page of auths if summary segment found. Inputs WS-ACCT-ID, outputs populated map fields via subordinates. No direct business decisions but chains to data retrieval. Errors propagated from calls. Calls GATHER-ACCOUNT-DETAILS, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PF7-KEY
Handles backward page navigation (PF7) if not page 1, decrements page num, repositions to prev page key, gets summary, initializes list, processes forward to redisplay current page. Inputs CDEMO-CPVS-PAGE-NUM/PREVPGS, outputs updated page data/message if at top. Logic checks page >1, sets NEXT-PAGE-YES/SEND-ERASE-NO. Errors via WS-MESSAGE if at top. Calls GET-AUTH-SUMMARY, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PF8-KEY
Handles forward page navigation (PF8) to last/next page, repositions to last key if set, gets summary, repositions auths, sets flags, processes forward if next available else message at bottom. Inputs CDEMO-CPVS-PAUKEY-LAST, outputs updated list/message. Logic determines if more pages via peek next. Calls GET-AUTH-SUMMARY, REPOSITION-AUTHORIZATIONS, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PAGE-FORWARD
Populates one page (5 auths) via loop: init index=1, until 5/EOF/ERR, reposition if PF7 first, else GNP next detail, populate list entry if success, track last/prev keys/page num, peek one extra for NEXT-PAGE-YES. Inputs current IMS position/WS-AUTH-KEY-SAVE, outputs 5 map list fields + commarea keys/page. Business logic limits to 5 per page, tracks navigation keys. Errors stop loop. Calls REPOSITION-AUTHORIZATIONS/GET-AUTHORIZATIONS, POPULATE-AUTH-LIST, GET-AUTHORIZATIONS.

### SEND-PAULST-SCREEN
Sends the populated output map COPAU0AO with header/message, ERASE if flag yes else refresh, CURSOR position. Inputs COPAU0AO fields, WS-MESSAGE, SEND-ERASE-FLG. Syncpoint IMS if PSB scheduled. No decisions, always sends. Calls POPULATE-HEADER-INFO.

### RECEIVE-PAULST-SCREEN
Receives input map COPAU0AI with RESP codes. Inputs none directly, outputs map fields/RESP. No logic or errors handled here.

### GATHER-ACCOUNT-DETAILS
Retrieves and displays account header info: xref, acct, cust reads, formats customer name/address/phone, credit limits, auth summary stats (counts/bals/amts) if found. Inputs WS-ACCT-ID, outputs map fields like CNAMEO, CREDLIMO, APPRCNTO. Chains file reads, formats strings. Errors send screen with message. Calls GETCARDXREF-BYACCT, GETACCTDATA-BYACCT, GETCUSTDATA-BYCUST, GET-AUTH-SUMMARY.

## Open Questions

- ? Exact layout/fields of copybooks like COCOM01Y, COPAU00, CIPAUSMY
  - Context: Inferred from usage but not fully defined in source
- ? Calling programs/transactions invoking CPVS
  - Context: Not specified in this source
