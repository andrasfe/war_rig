# COPAUS0C

**File**: `cbl/COPAUS0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 14:22:43.300258

## Purpose

COPAUS0C is a CICS online program that provides a paginated summary view of pending authorization details for a specific account ID in the CardDemo authorization module. It retrieves account, customer, and card cross-reference data from VSAM files, fetches IMS pending authorization summary and detail segments, populates a BMS map with header info, customer details, summary counts, and a list of up to 5 authorizations, and handles navigation (PF7/PF8), selection for detail view (ENTER), and return to menu (PF3).

**Business Context**: Serves the CardDemo application by displaying authorization message summaries to allow users to review and select pending authorizations for an account.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AI | IOType.CICS_MAP | BMS input map containing entered account ID (ACCTIDI) and selection flags (SEL0001I-SEL0005I) from user screen interactions |
| CXACAIX | IOType.FILE_VSAM | Card cross-reference alternate index by account ID to retrieve customer ID and card number |
| ACCTDAT | IOType.FILE_VSAM | Account master file containing account details like credit limits |
| CUSTDAT | IOType.FILE_VSAM | Customer master file containing customer name, address, phone |
| PAUTSUM0 | IOType.IMS_SEGMENT | Pending authorization summary root segment containing counts and balances for the account |
| PAUTDTL1 | IOType.IMS_SEGMENT | Pending authorization details child segments containing individual authorization records |
| DFHCOMMAREA | IOType.CICS_COMMAREA | Common CardDemo commarea passed between programs containing account ID, page keys, selected auth key |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | BMS output map populated with header, customer info, account summary stats, authorization list, and error messages |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Updated commarea with page navigation keys, selected authorization, page number for return or XCTL |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS1C | CallType.CICS_XCTL | Transfer control to authorization detail program when user selects 'S' on an authorization line |
| COMEN01C | CallType.CICS_XCTL | Return to menu program on PF3 key press |
| COSGN00C | CallType.CICS_XCTL | Default fallback program for return if no previous program specified |

## Business Rules

- **BR001**: Account ID must be numeric and not spaces/low-values for processing
- **BR002**: Authorization selection must be 'S' or 's' to proceed to detail view
- **BR003**: Approval status 'A' if response code '00', else 'D' for display
- **BR004**: Page navigation backward only if current page >1; forward repositions from last key

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph that initializes flags, handles first-time entry (EIBCALEN=0) by sending initial screen, or processes commarea for reentry. It consumes EIBCALEN, DFHCOMMAREA if present, and map data via RECEIVE-PAULST-SCREEN. It produces updated COPAU0AO map and CARDDEMO-COMMAREA for return or XCTL. Business logic evaluates EIBAID for PF/ENTER keys dispatching to specific handlers like PROCESS-ENTER-KEY, navigation, or return; first distinguishes initial load vs reenter. Errors set WS-ERR-FLG and message. Calls RECEIVE-PAULST-SCREEN, PROCESS-ENTER-KEY, SEND-PAULST-SCREEN, RETURN-TO-PREV-SCREEN, GATHER-DETAILS, and CICS RETURN with TRANSID CPVS. Always ends with CICS RETURN preserving commarea and transaction.

### PROCESS-ENTER-KEY
Handles ENTER key press to validate and process account ID entry or line selection for detail view. Consumes ACCTIDI and SEL0001I-SEL0005I from input map COPAU0AI. Produces updated CDEMO-CPVS-PAU-SEL-FLG, CDEMO-CPVS-PAU-SELECTED in commarea, WS-ACCT-ID, and error message if invalid. Business logic checks ACCTIDI for spaces/low/non-numeric, maps SEL fields to index 1-5 for selected auth key, validates selection 'S'/'s' before XCTL to detail program. Errors set WS-ERR-FLG, cursor to ACCTIDL, custom messages. Calls GATHER-DETAILS to refresh data post-processing.

### GATHER-DETAILS
Orchestrates data retrieval and page population for the current account. Consumes WS-ACCT-ID from map/commarea. Produces populated auth list in map via PROCESS-PAGE-FORWARD, initializes data. Business logic conditionally gathers account details if valid ACCT-ID, initializes auth data, processes first page of auths if summary segment found. No explicit errors but relies on subordinate error handling. Calls GATHER-ACCOUNT-DETAILS, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PF7-KEY
Handles backward page navigation (PF7). Consumes CDEMO-CPVS-PAGE-NUM and PAUKEY-PREV-PG array from commarea. Produces updated page num, WS-AUTH-KEY-SAVE from prev key, refreshed list via PROCESS-PAGE-FORWARD. Business logic decrements page if >1, loads prev key, calls GET-AUTH-SUMMARY and PROCESS-PAGE-FORWARD; else error message. Sets SEND-ERASE-NO and NEXT-PAGE-YES. Calls GET-AUTH-SUMMARY, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PF8-KEY
Handles forward page navigation to next page (PF8). Consumes CDEMO-CPVS-PAUKEY-LAST from commarea. Produces repositioned auth list via REPOSITION-AUTHORIZATIONS and PROCESS-PAGE-FORWARD. Business logic uses last key for GNP reposition if valid, sets NEXT-PAGE-YES/NO based on further read; error if at end. Sets SEND-ERASE-NO. Calls GET-AUTH-SUMMARY, REPOSITION-AUTHORIZATIONS, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PAGE-FORWARD
Fills the current page with up to 5 authorization details by sequential GNP reads. Consumes current IMS position/WS-AUTH-KEY-SAVE. Produces CDEMO-CPVS-AUTH-KEYS(1-5), PAUKEY-LAST/PREV-PG, map fields TRNIDxxI/PDATExxI etc via POPULATE-AUTH-LIST, sets NEXT-PAGE-YES if more. Business logic loops WS-IDX 1-5 calling GET-AUTHORIZATIONS or REPOSITION for first, populates if no EOF/error, peeks one extra for next flag. Handles PF7 reposition special case. Calls REPOSITION-AUTHORIZATIONS, GET-AUTHORIZATIONS, POPULATE-AUTH-LIST.

### GET-AUTHORIZATIONS
Performs IMS GNP to fetch next PAUTDTL1 detail segment. Consumes current PCB position. Produces PENDING-AUTH-DETAILS populated. Business logic executes DLI GNP PAUTDTL1, evaluates IMS-RETURN-CODE: OK continues, GE/GB sets EOF, others error with message and SEND. No subordinate calls.

### REPOSITION-AUTHORIZATIONS
Repositions IMS cursor to specific authorization key for paging. Consumes WS-AUTH-KEY-SAVE moved to PA-AUTHORIZATION-KEY. Produces PENDING-AUTH-DETAILS at key. Business logic DLI GNP PAUTDTL1 WHERE PAUT9CTS=KEY, evaluates code: OK/AUTHS-NOT-EOF, GE/GB EOF, other error/SEND. No calls.

### POPULATE-AUTH-LIST
Transforms and maps current detail segment to screen list fields for current WS-IDX (1-5). Consumes PENDING-AUTH-DETAILS fields like PA-APPROVED-AMT, dates, resp code. Produces map fields TRNIDxxI, PAMTxxI, CDEMO-CPVS-AUTH-KEYS(WS-IDX). Business logic formats time/date, sets status A/D, EVALUATE WS-IDX to assign specific map fields and unprotected attrs. No errors or calls.

### INITIALIZE-AUTH-DATA
Clears the 5 authorization list fields in map to protected blanks before population. Consumes nothing specific. Produces SPACES in TRNIDxxI/PDATE etc, DFHBMPRO in SEL000xA. Business logic VARYING WS-IDX 1-5 EVALUATE to set each group of fields. No errors/calls.

### RETURN-TO-PREV-SCREEN
Prepares XCTL to previous or menu program on PF3. Consumes CDEMO-TO-PROGRAM (defaults COSGN00C). Produces updated commarea context flags. Business logic sets from/to programs, context 0, XCTL PROGRAM. No errors.

### SEND-PAULST-SCREEN
Sends the populated map to terminal with optional ERASE. Consumes fully populated COPAU0AO incl message/header. Produces screen display. Business logic syncpoint if PSB scheduled, calls POPULATE-HEADER-INFO, SEND MAP COPAU0A ERASE if flag or CURSOR only. No errors.

### RECEIVE-PAULST-SCREEN
Receives user input from map into COPAU0AI with resp codes. Consumes screen data. Produces COPAU0AI fields populated, WS-RESP-CD/REAS. Business logic CICS RECEIVE MAP. No further logic.

### POPULATE-HEADER-INFO
Fills static header fields with titles, tran/prog names, current date/time. Consumes FUNCTION CURRENT-DATE. Produces TITLE01O-02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO in map. Business logic formats date/time from WS-CURDATE-*, moves constants. No errors/calls.

### GATHER-ACCOUNT-DETAILS
Retrieves and displays account/customer summary info from VSAM files and IMS summary. Consumes WS-ACCT-ID. Produces map fields CNAMEO, ADDR001O-002O, CREDLIMO, APPRCNTO etc from records/IMS. Business logic calls file reads, formats name/address, moves limits/balances/counts if summary found else zeros. Calls GET-AUTH-SUMMARY.

### GETCARDXREF-BYACCT
Reads card xref VSAM via alt index for cust/card nums. Consumes WS-ACCT-ID to RID. Produces CARD-XREF-RECORD, CDEMO-CUST-ID/CARD-NUM. Business logic CICS READ, NORMAL moves fields, NOTFND/OTHER error message/SEND.

### GETACCTDATA-BYACCT
Reads account VSAM by acct ID from xref. Consumes XREF-ACCT-ID to RID. Produces ACCOUNT-RECORD. Business logic CICS READ, NORMAL continue, NOTFND/OTHER error/SEND.

### GETCUSTDATA-BYCUST
Reads customer VSAM by cust ID from xref. Consumes XREF-CUST-ID to RID. Produces CUSTOMER-RECORD. Business logic CICS READ, NORMAL continue, NOTFND/OTHER error/SEND.

### GET-AUTH-SUMMARY
Schedules PSB and GU IMS summary segment by acct ID. Consumes CDEMO-ACCT-ID to PA-ACCT-ID. Produces PENDING-AUTH-SUMMARY if found, sets FOUND-PAUT-SMRY-SEG flag. Business logic SCHEDULE-PSB, DLI GU WHERE ACCNTID=, OK found, GE notfound, other error/SEND.

### SCHEDULE-PSB
Schedules IMS PSB 'PSBPAUTB' handling duplicates. Consumes PSB-NAME. Produces scheduled PCB, sets IMS-PSB-SCHD flag. Business logic DLI SCHD NODHABEND, if TC TERM+SCHD again, OK set flag else error/SEND.

## Open Questions

- ? Exact field layouts and meanings in copybooks like CIPAUSMY, CVACT01Y
  - Context: Copybooks not provided in source; inferred from usage but precise types/sizes unknown
- ? Calling programs and full navigation flow (e.g. who calls CPVS)
  - Context: No self-references; commarea suggests from menu like COMEN01C
