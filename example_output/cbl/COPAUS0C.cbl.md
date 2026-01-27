# COPAUS0C

**File**: `cbl/COPAUS0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-27 02:41:13.114589

## Purpose

COPAUS0C is a CICS/IMS/BMS online program that provides a paginated summary view of pending authorization details for a specific account ID entered by the user. It retrieves related customer and account data from VSAM files, fetches authorization summary and detail segments from an IMS database using DLI calls, populates a BMS map with up to 5 authorization records per page, and supports navigation (PF7 backward, PF8 forward) and selection (Enter key with 'S') to XCTL to detail view COPAUS1C. PF3 returns to the menu program.

**Business Context**: Serves the CardDemo authorization module by displaying summary views of pending authorizations to allow users to review and select for details in a credit card processing system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AI | IOType.CICS_MAP | BMS input map containing user-entered account ID (ACCTIDI), selection flags (SEL0001I-SEL0005I), and other screen fields received via CICS RECEIVE. |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Common commarea holding account ID (CDEMO-ACCT-ID), authorization keys (CDEMO-CPVS-AUTH-KEYS), page info (CDEMO-CPVS-PAGE-NUM, CDEMO-CPVS-PAUKEY-PREV-PG), selection flags, and navigation flags passed between programs. |
| CXACAIX | IOType.FILE_VSAM | Card cross-reference file accessed by alternate index on account ID to retrieve customer ID and card number. |
| ACCTDAT | IOType.FILE_VSAM | Account master file read by account ID to retrieve account details like credit limits. |
| CUSTDAT | IOType.FILE_VSAM | Customer master file read by customer ID to retrieve customer name, address, phone. |
| PAUTSUM0 | IOType.IMS_SEGMENT | IMS root segment for pending authorization summary, qualified by account ID, containing counts and totals for approved/declined auths. |
| PAUTDTL1 | IOType.IMS_SEGMENT | IMS child segment for pending authorization details, read sequentially or repositioned by key, containing individual auth details like amount, date, status. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | BMS output map populated with header info, customer/account details, authorization list (5 records with txn ID, date, time, type, status, amount), counts/totals, and error messages, sent via CICS SEND. |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Updated with current page num, prev page keys, last key, selected auth key, acct ID before RETURN TRANSID. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS1C | CallType.CICS_XCTL | Transfer control to authorization details program when user selects an auth record with 'S' on Enter. |
| COMEN01C | CallType.CICS_XCTL | Return to menu program on PF3. |

## Business Rules

- **BR001**: Account ID must be numeric and non-blank for processing; otherwise display error message.
- **BR002**: Authorization selection must be 'S' or 's' to XCTL to details; other values show invalid selection error.
- **BR003**: Approval status derived from response code: 'A' if '00', else 'D'.
- **BR004**: Paging backward (PF7) only if current page >1, using saved prev page key.

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph that initializes flags, handles first-time entry vs reentry via commarea, receives/sends BMS maps, and dispatches to key handlers based on AID (Enter, PF3, PF7, PF8). It consumes EIBCALEN, DFHCOMMAREA, and map inputs (COPAU0AI), producing updated commarea and screen output (COPAU0AO). Business logic evaluates EIBAID to route to process-enter, pf7/pf8 navigation, or menu return; on first entry (EIBCALEN=0), initializes and sends initial screen. Error handling sets WS-ERR-FLG for invalid keys and displays messages. It calls RECEIVE-PAULST-SCREEN, GATHER-DETAILS, SEND-PAULST-SCREEN, and XCTL/RETURN as needed. Always ends with CICS RETURN TRANSID CPVS preserving commarea.

### PROCESS-ENTER-KEY
Handles Enter key press by validating and processing account ID input and any selection (SEL0001I-SEL0005I) for detail view transfer. Consumes ACCTIDI and SEL fields from COPAU0AI, sets WS-ACCT-ID and CDEMO-CPVS-PAU-SELECTED based on which SEL is non-blank. Outputs updated commarea fields and potentially XCTLs to COPAUS1C if valid 'S' selection. Business logic: validates acct numeric/non-blank, maps selection to auth key index 1-5, checks flag='S'/'s' for XCTL with context flags. Errors for invalid acct/selection set WS-MESSAGE and clear input field. Calls GATHER-DETAILS to refresh list post-processing.

### GATHER-DETAILS
Orchestrates data retrieval for screen refresh by account ID, initializing page and calling file/IMS reads if acct valid. Consumes WS-ACCT-ID from input/commarea, produces populated map fields via sub-calls. Business logic: if acct valid, gathers acct/cust/xref details, IMS summary, then loads first page of details. No direct errors but propagates from sub-paragraphs. Calls GATHER-ACCOUNT-DETAILS, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PF7-KEY
Handles backward paging (PF7) by decrementing page num if >1, repositioning to prev page's first key, and refreshing page. Consumes CDEMO-CPVS-PAGE-NUM and PAUKEY-PREV-PG array, sets WS-AUTH-KEY-SAVE. Outputs refreshed screen without erase. Business logic checks page>1, repositions IMS, sets NEXT-PAGE-YES. Error/message if already at top. Calls GET-AUTH-SUMMARY, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PF8-KEY
Handles forward paging to next page (PF8) by repositioning to last key if available and loading next page. Consumes CDEMO-CPVS-PAUKEY-LAST, sets WS-AUTH-KEY-SAVE. Business logic repositions if last key valid, sets NEXT-PAGE-YES/NO based on further read. Error message if already at end. Calls GET-AUTH-SUMMARY, REPOSITION-AUTHORIZATIONS, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PAGE-FORWARD
Loads current page of up to 5 auth details by looping GET-AUTHORIZATIONS or REPOSITION for first, populating list until 5 or EOF. Consumes IMS PCB via DLI, WS-IDX=1 start. Produces CDEMO-CPVS-AUTH-KEYS array, map fields TRNIDnnI/PDATEnnI etc., page num, prev/last keys. Business logic loop until IDX>5/EOF/ERR, peeks one extra for NEXT-PAGE-YES. Calls REPOSITION-AUTHORIZATIONS (PF7), GET-AUTHORIZATIONS, POPULATE-AUTH-LIST.

### GET-AUTHORIZATIONS
Performs unqualified GNP DLI call to fetch next PAUTDTL1 child segment into PENDING-AUTH-DETAILS. Consumes current IMS position/PCB. Sets AUTHS-EOF or error based on DIBSTAT. Error handling: non-OK/GE/GB sets ERR-FLG and error message, sends screen. No outputs beyond global IMS vars.

### REPOSITION-AUTHORIZATIONS
Repositions IMS cursor for paging by GU DLI with WHERE on PA-AUTHORIZATION-KEY = WS-AUTH-KEY-SAVE for PAUTDTL1. Consumes WS-AUTH-KEY-SAVE. Sets EOF or error on DIBSTAT. Error handling similar to GET-AUTHORIZATIONS.

### POPULATE-AUTH-LIST
Transforms current PENDING-AUTH-DETAILS segment data into display fields for the current WS-IDX (1-5) on screen map and commarea. Consumes PA- fields like APPROVED-AMT, AUTH-ORIG-TIME/DATE, RESP-CODE, TRANSACTION-ID. Outputs map fields like TRNID01I-PAMT005I, PAPRVnnI, CDEMO-CPVS-AUTH-KEYS(n). Business logic formats date/time, derives approval 'A'/'D' from resp code, EVALUATE WS-IDX to assign to specific fields.

### INITIALIZE-AUTH-DATA
Clears the 5 authorization list fields on output map (TRNIDnnI etc.) and sets protected attributes DFHBMPRO before loading new page. Consumes nothing directly, loops WS-IDX 1-5. Outputs cleared SPACES to all list fields.

### RETURN-TO-PREV-SCREEN
Prepares XCTL to previous menu program (COMEN01C or fallback COSGN00C) on PF3, setting context flags. Consumes CDEMO-TO-PROGRAM, sets FROM fields. No error handling.

### SEND-PAULST-SCREEN
Sends the populated COPAU0AO map to screen with/without ERASE based on flag, after IMS syncpoint if PSB scheduled. Consumes COPAU0AO fields, WS-MESSAGE, SEND-ERASE-FLG. Calls POPULATE-HEADER-INFO first, handles IMS-PSB-SCHD cleanup.

### RECEIVE-PAULST-SCREEN
Receives user input into COPAU0AI mapset/map with RESP codes. Consumes screen data via CICS RECEIVE.

### POPULATE-HEADER-INFO
Fills static header fields on output map with current date/time, tranid, pgmname, titles from copybooks. Consumes FUNCTION CURRENT-DATE, WS-CICS-TRANID etc.

### GATHER-ACCOUNT-DETAILS
Reads VSAM files for acct/cust/xref by WS-ACCT-ID, populates customer name/address/credit limits/summary counts on map. Consumes WS-ACCT-ID. Business logic STRING for name/address, moves limits/counts/balances from IMS summary if found. Calls file read paragraphs and GET-AUTH-SUMMARY.

### GETCARDXREF-BYACCT
CICS READ VSAM CXACAIX by acct ID RID into CARD-XREF-RECORD, sets CDEMO-CUST-ID/CARD-NUM. Error handling for NOTFND/OTHER: message and send screen.

### GETACCTDATA-BYACCT
CICS READ VSAM ACCTDAT by acct ID into ACCOUNT-RECORD for credit limits. Error handling similar.

### GETCUSTDATA-BYCUST
CICS READ VSAM CUSTDAT by cust ID from xref into CUSTOMER-RECORD for name/address/phone. Error handling similar.

### GET-AUTH-SUMMARY
Schedules PSB if needed, then GU DLI for PAUTSUM0 by PA-ACCT-ID into PENDING-AUTH-SUMMARY, sets found flags. Error handling for IMS codes.

### SCHEDULE-PSB
DLI SCHD PSBPAUTB with retry TERM/SCHD if already scheduled, sets IMS-PSB-SCHD flag. Error handling sends screen.

## Control Flow

```mermaid
flowchart TD
    %% Title: COPAUS0C.cbl
    GATHER_ACCOUNT_DETAILS["GATHER-ACCOUNT-DETAILS"]
    GET_AUTH_SUMMARY["GET-AUTH-SUMMARY"]
    GETACCTDATA_BYACCT["GETACCTDATA-BYACCT"]
    GETCARDXREF_BYACCT["GETCARDXREF-BYACCT"]
    GETCUSTDATA_BYCUST["GETCUSTDATA-BYCUST"]
    GATHER_DETAILS["GATHER-DETAILS"]
    INITIALIZE_AUTH_DATA["INITIALIZE-AUTH-DATA"]
    PROCESS_PAGE_FORWARD["PROCESS-PAGE-FORWARD"]
    SCHEDULE_PSB["SCHEDULE-PSB"]
    SEND_PAULST_SCREEN["SEND-PAULST-SCREEN"]
    GET_AUTHORIZATIONS["GET-AUTHORIZATIONS"]
    WS_ACCTFILENAME__ext[("WS-ACCTFILENAME")]
    WS_CARDXREFNAME_ACCT_PATH__ext[("WS-CARDXREFNAME-ACCT-PATH")]
    WS_CUSTFILENAME__ext[("WS-CUSTFILENAME")]
    MAIN_PARA["MAIN-PARA"]
    PROCESS_ENTER_KEY["PROCESS-ENTER-KEY"]
    PROCESS_PF7_KEY["PROCESS-PF7-KEY"]
    PROCESS_PF8_KEY["PROCESS-PF8-KEY"]
    RECEIVE_PAULST_SCREEN["RECEIVE-PAULST-SCREEN"]
    RETURN_TO_PREV_SCREEN["RETURN-TO-PREV-SCREEN"]
    POPULATE_AUTH_LIST["POPULATE-AUTH-LIST"]
    POPULATE_HEADER_INFO["POPULATE-HEADER-INFO"]
    CDEMO_TO_PROGRAM__ext(["CDEMO-TO-PROGRAM"])
    REPOSITION_AUTHORIZATIONS["REPOSITION-AUTHORIZATIONS"]
    GATHER_ACCOUNT_DETAILS --> GET_AUTH_SUMMARY
    GATHER_ACCOUNT_DETAILS --> GETACCTDATA_BYACCT
    GATHER_ACCOUNT_DETAILS --> GETCARDXREF_BYACCT
    GATHER_ACCOUNT_DETAILS --> GETCUSTDATA_BYCUST
    GATHER_DETAILS --> GATHER_ACCOUNT_DETAILS
    GATHER_DETAILS --> INITIALIZE_AUTH_DATA
    GATHER_DETAILS --> PROCESS_PAGE_FORWARD
    GET_AUTH_SUMMARY --> SCHEDULE_PSB
    GET_AUTH_SUMMARY --> SEND_PAULST_SCREEN
    GET_AUTHORIZATIONS --> SEND_PAULST_SCREEN
    GETACCTDATA_BYACCT --> SEND_PAULST_SCREEN
    GETACCTDATA_BYACCT -.->|reads| WS_ACCTFILENAME__ext
    GETCARDXREF_BYACCT --> SEND_PAULST_SCREEN
    GETCARDXREF_BYACCT -.->|reads| WS_CARDXREFNAME_ACCT_PATH__ext
    GETCUSTDATA_BYCUST --> SEND_PAULST_SCREEN
    GETCUSTDATA_BYCUST -.->|reads| WS_CUSTFILENAME__ext
    MAIN_PARA --> GATHER_DETAILS
    MAIN_PARA --> PROCESS_ENTER_KEY
    MAIN_PARA --> PROCESS_PF7_KEY
    MAIN_PARA --> PROCESS_PF8_KEY
    MAIN_PARA --> RECEIVE_PAULST_SCREEN
    MAIN_PARA --> RETURN_TO_PREV_SCREEN
    MAIN_PARA --> SEND_PAULST_SCREEN
    PROCESS_ENTER_KEY --> GATHER_DETAILS
    PROCESS_ENTER_KEY -.->|calls| CDEMO_TO_PROGRAM__ext
    PROCESS_PAGE_FORWARD --> GET_AUTHORIZATIONS
    PROCESS_PAGE_FORWARD --> POPULATE_AUTH_LIST
    PROCESS_PAGE_FORWARD --> REPOSITION_AUTHORIZATIONS
    PROCESS_PF7_KEY --> GET_AUTH_SUMMARY
    PROCESS_PF7_KEY --> INITIALIZE_AUTH_DATA
    PROCESS_PF7_KEY --> PROCESS_PAGE_FORWARD
    PROCESS_PF8_KEY --> GET_AUTH_SUMMARY
    PROCESS_PF8_KEY --> INITIALIZE_AUTH_DATA
    PROCESS_PF8_KEY --> PROCESS_PAGE_FORWARD
    PROCESS_PF8_KEY --> REPOSITION_AUTHORIZATIONS
    REPOSITION_AUTHORIZATIONS --> SEND_PAULST_SCREEN
    RETURN_TO_PREV_SCREEN -.->|calls| CDEMO_TO_PROGRAM__ext
    SCHEDULE_PSB --> SEND_PAULST_SCREEN
    SEND_PAULST_SCREEN --> POPULATE_HEADER_INFO
```

## Open Questions

- ? Exact field layouts in copybooks like COPAU00, CIPAUSMY
  - Context: Source uses fields like ACCTIDI, PA-APPROVED-AMT but definitions in COPY not provided
- ? How called_by programs invoke this (e.g. from menu)
  - Context: No IDENTIFICATION DIVISION caller info; inferred from commarea CDEMO-FROM-PROGRAM
