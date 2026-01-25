# COPAUS0C

**File**: `cbl/COPAUS0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-25 18:32:43.720040

## Purpose

CICS online program displaying summary view of pending authorization messages for an account, allowing selection for details view via 'S', pagination with PF7/PF8, and return to menu with PF3. Retrieves account/customer/card xref data from VSAM files and authorization summary/details from IMS database. Populates BMS map COPAU0A with up to 5 auth records per page.

**Business Context**: CardDemo application Authorization Module providing summary list of authorizations tied to credit card accounts.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | Input map fields for account ID (ACCTIDI), selection fields (SEL0001I-SEL0005I), received via CICS RECEIVE |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Common commarea containing CDEMO-ACCT-ID, CDEMO-CPVS-AUTH-KEYS, page num, prev keys, selection flag from prior calls |
| PAUTSUM0 | IOType.IMS_SEGMENT | Pending Authorization Summary root segment qualified by account ID |
| PAUTDTL1 | IOType.IMS_SEGMENT | Pending Authorization Details child segment retrieved sequentially or repositioned by key |
| CXACAIX | IOType.FILE_VSAM | Card Xref alternate index by account ID to get cust ID and card num |
| ACCTDAT | IOType.FILE_VSAM | Account master file read by account ID |
| CUSTDAT | IOType.FILE_VSAM | Customer master file read by customer ID |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | Output map populated with account details, customer info, auth summary counts/balances, and list of 5 auth records (trans ID, date, time, type, status, amount) |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Updated commarea with auth keys, page info, selection for return and XCTL |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS1C | CallType.CICS_XCTL | Transfer control to authorization details program when 'S' selected on list item |
| COMEN01C | CallType.CICS_XCTL | Return to menu screen on PF3 |
| COSGN00C | CallType.CICS_XCTL | Fallback to signon screen if no prior program in commarea on PF3 |

## Business Rules

- **BR001**: Account ID must be numeric and non-blank to proceed with data gathering
- **BR002**: Selection field (SELxxxxI) must be 'S' or 's' to XCTL to details, else invalid selection message
- **BR003**: Display up to 5 auth details per page, track prev page keys and last key for pagination
- **BR004**: Approval status 'A' if resp code '00', else 'D' for display

## Paragraphs/Procedures

### MAIN-PARA
Main control: initialize, handle first entry vs reentry, receive map, evaluate AID (ENTER/PF3/PF7/PF8), process, send map, return TRANSID CPVS

### PROCESS-ENTER-KEY
Validate acct ID numeric, handle selection SELxxxxI to set selected auth key and XCTL to detail if 'S', regather details

### GATHER-DETAILS
Gather account details (files + IMS summary), init auth data, process page forward to populate list

### PROCESS-PF7-KEY
Previous page: decrement page num, reposition to prev key, process page forward

### PROCESS-PF8-KEY
Next page: reposition to last key if avail, process page forward

### PROCESS-PAGE-FORWARD
Populate 5 auth records via GET-AUTHORIZATIONS or REPOSITION, POPULATE-AUTH-LIST, check for next page

### GATHER-ACCOUNT-DETAILS
Read xref/acct/cust VSAM, populate map header with cust name/addr/phone/limits, get IMS summary for counts/balances

### SEND-PAULST-SCREEN
Populate header (titles/date/time), send map ERASE if flag or CURSOR

## Open Questions

- ? Exact field layouts and lengths in copybooks (e.g. PA-ACCT-ID length matches WS-ACCT-ID PIC X(11)?)
  - Context: Copybooks not provided inline, assumed compatible from usage but cannot verify precise mappings
- ? Programs that XCTL to this (calling_context.called_by)
  - Context: No static linkage or JCL, determined dynamically via commarea CDEMO-FROM-PROGRAM but no callers evident
- ? CCXREF file defined but unused?
  - Context: WS-CCXREF-FILE line 42 defined but no READ/usage found
