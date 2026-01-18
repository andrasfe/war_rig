# COCRDSLC

**File:** COCRDSLC.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 2
**Analyzed:** 2026-01-18 17:15:19.300515

## Purpose

CICS online program that presents a screen for entering or pre-filling account ID and card number to retrieve and display specific credit card details from the CARDDAT VSAM file. Validates user inputs for numeric format and length, handles PF keys for exit or re-display, and returns control via XCTL or CICS RETURN with updated COMMAREA. Supports invocation from menu or card list screen with pre-validated criteria.

**Business Context:** Credit card inquiry subsystem allowing lookup of individual card details by account/card combination or account.
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 4, 24, 166, 248, 304

## Calling Context

**Called By:** [COMEN01C](./COMEN01C.md), [COCRDLIC](./COCRDLIC.md)
**Entry Points:** CCDL
**Linkage Section:** DFHCOMMAREA

## Inputs

### CCRDSLAI
- **Type:** CICS_MAP
- **Description:** Screen input fields for account ID (ACCTSIDI) and card number (CARDSIDI) from user or prior COMMAREA
- **Copybook:** [COCRDSL](../copybooks/COCRDSL.md)
- **Lines:** 597, 614, 622

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Passed from calling program containing context like FROM-PROGRAM, FROM-TRANID, ACCT-ID, CARD-NUM
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 243, 268, 274

### CARDDAT
- **Type:** FILE_VSAM
- **Description:** VSAM file containing CARD-RECORD details accessed by card number (primary) or account ID (alternate index CARDAIX)
- **Copybook:** [CVACT02Y](../copybooks/CVACT02Y.md)
- **Lines:** 187, 742, 783

### EIBAID
- **Type:** CICS_COMMAREA
- **Description:** CICS attention key from DFHAID copybook used to determine user action like ENTER or PF3
- **Copybook:** [DFHAID](../copybooks/DFHAID.md)
- **Lines:** 209, 284, 292

## Outputs

### CCRDSLAO
- **Type:** CICS_MAP
- **Description:** Screen output fields populated with titles, dates, input echoes, card details (name, expiry, status), error/info messages
- **Copybook:** [COCRDSL](../copybooks/COCRDSL.md)
- **Lines:** 569, 432, 475, 494

### WS-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated COMMAREA with context (TO-PROGRAM, TO-TRANID), validated ACCT-ID, CARD-NUM, LAST-MAPSET/MAP returned to transaction
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 397, 402

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | Transfer control to calling program (e.g., menu COMEN01C or list COCRDLIC) on PF3 exit, passing updated COMMAREA | 331 |

## Business Rules

### BR001: Account ID must be exactly 11 numeric digits and non-zero if provided
**Logic:** Check IS NUMERIC, length implicit via PIC 9(11), ZEROS check; set error flags and message if invalid
**Conditions:** CC-ACCT-ID EQUAL LOW-VALUES OR SPACES OR ZEROS, CC-ACCT-ID NOT NUMERIC
**Lines:** 650, 665

### BR002: Card number must be exactly 16 numeric digits if provided
**Logic:** Check IS NUMERIC on CC-CARD-NUM-N PIC 9(16); set error flags and message if invalid or blank
**Conditions:** CC-CARD-NUM EQUAL LOW-VALUES OR SPACES OR ZEROS, CC-CARD-NUM NOT NUMERIC
**Lines:** 691, 706

### BR003: No search criteria (both account and card blank) triggers 'No input received' error
**Logic:** Cross-field check after individual edits
**Conditions:** FLG-ACCTFILTER-BLANK AND FLG-CARDFILTER-BLANK
**Lines:** 637

### BR004: Read CARDDAT by primary key (card number) succeeds only if exact account+card match found; display details or error
**Logic:** CICS READ with RIDFLD cardnum on CARDDAT; NORMAL sets FOUND-CARDS-FOR-ACCOUNT
**Conditions:** DFHRESP(NORMAL), DFHRESP(NOTFND)
**Lines:** 752

### BR005: Alternate index read by account ID on CARDAIX for cases where only account provided (prepared logic)
**Logic:** CICS READ with RIDFLD acct-id on CARDAIX file path; NORMAL sets FOUND-CARDS-FOR-ACCOUNT, NOTFND sets account not found error
**Conditions:** DFHRESP(NORMAL), DFHRESP(NOTFND)
**Lines:** 793, 783

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVCRD01Y](../copybooks/CVCRD01Y.md) | WORKING_STORAGE | Credit card work area (CC-WORK-AREA) initialization and usage | 194 |
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | CARDDEMO-COMMAREA structure for program context, account/card data across calls | 198 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | CICS BMS map attribute constants (DFHBMFSE, DFHBMPRF, colors) | 208 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | EIBAID values for attention keys (ENTER, PFK03) | 209 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Screen titles (CCDA-TITLE01, TITLE02) | 213 |
| [COCRDSL](../copybooks/COCRDSL.md) | WORKING_STORAGE | Defines screen map CCRDSLAI/O for account/card input/output, error/info messages | 215 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Current date/time fields (WS-CURDATE-DATA) | 218 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Common messages | 221 |
| [CSMSG02Y](../copybooks/CSMSG02Y.md) | WORKING_STORAGE | Abend variables (ABEND-DATA, ABEND-MSG) | 224 |
| [CSUSR01Y](../copybooks/CSUSR01Y.md) | WORKING_STORAGE | Signed on user data | 227 |
| [CVACT02Y](../copybooks/CVACT02Y.md) | WORKING_STORAGE | CARD-RECORD layout for VSAM file data including embossed name, expiry, status | 234 |
| [CVCUS01Y](../copybooks/CVCUS01Y.md) | WORKING_STORAGE | Customer layout (unused in visible logic) | 240 |

## Data Flow

### Reads From
- **CARDDAT**: CARD-RECORD
  (Lines: 746)
- **CCRDSLAI**: ACCTSIDI, CARDSIDI
  (Lines: 619, 626)
- **DFHCOMMAREA**: CDEMO-ACCT-ID, CDEMO-CARD-NUM, CDEMO-FROM-PROGRAM, CDEMO-PGM-ENTER
  (Lines: 342, 268)

### Writes To
- **CCRDSLAO**: ACCTSIDO, CRDNAMEO, EXPMONO, EXPYEARO, CRDSTCDO, ERRMSGO, INFOMSGO
  (Lines: 465, 476, 494)
- **CARDDEMO-COMMAREA**: CDEMO-ACCT-ID, CDEMO-CARD-NUM, CDEMO-TO-PROGRAM, CDEMO-TO-TRANID, CDEMO-LAST-MAPSET, CDEMO-LAST-MAP
  (Lines: 676, 328)

### Transformations
- **ACCTSIDI** → **CC-ACCT-ID**: Move screen input to working field, replace * or SPACES with LOW-VALUES
  (Lines: 619)
- **CC-ACCT-ID** → **CDEMO-ACCT-ID**: Validated numeric account ID moved to COMMAREA
  (Lines: 676)
- **CARD-EMBOSSED-NAME** → **CRDNAMEO**: Move file record embossed name to screen output if found
  (Lines: 476)
- **CARD-EXPIRAION-DATE** → **EXPMONO/EXPYEARO**: Parse expiry date from file to screen month/year fields
  (Lines: 478, 481)
- **WS-RETURN-MSG** → **ERRMSGO**: Move error message to screen error field
  (Lines: 494)
- **CDEMO-FROM-PROGRAM** → **CDEMO-TO-PROGRAM**: Copy from-program to to-program for XCTL on exit
  (Lines: 320)

## Key Paragraphs

### 0-MAIN
**Purpose:** Main control: initialize, store PFKEY/AID, evaluate context and AID to route to send map, process inputs, read data, or XCTL exit
- Calls: YYYY-STORE-PFKEY, 9000-READ-DATA, 0-SEND-MAP, 0-PROCESS-INPUTS
- Lines: 248-393

### COMMON-RETURN
**Purpose:** Set final error msg, build return COMMAREA, CICS RETURN to same TRANID
- Called by: 0-MAIN
- Lines: 394-407

### 0-SEND-MAP
**Purpose:** Chain to init/setup/send screen sequence
- Called by: 0-MAIN
- Calls: 0-SCREEN-INIT, 0-SETUP-SCREEN-VARS, 0-SETUP-SCREEN-ATTRS, 0-SEND-SCREEN
- Lines: 412-422

### 0-PROCESS-INPUTS
**Purpose:** Receive map, edit inputs, set next map/prog for error re-display
- Called by: 0-MAIN
- Calls: 0-RECEIVE-MAP, 0-EDIT-MAP-INPUTS
- Lines: 582-592

### 0-EDIT-MAP-INPUTS
**Purpose:** Move screen to working fields, perform account/card edits, cross-field no-input check
- Called by: 0-PROCESS-INPUTS
- Calls: 0-EDIT-ACCOUNT, 0-EDIT-CARD
- Lines: 608-642

### 0-READ-DATA
**Purpose:** Perform card lookup by account+card
- Called by: 0-MAIN
- Calls: 0-GETCARD-BYACCTCARD
- Lines: 726-731

### ABEND-ROUTINE
**Purpose:** Display abend data and ABEND CICS with 9999
- Lines: 857-878

## Error Handling

- **Any CICS ABEND:** Handled by ABEND-ROUTINE: send ABEND-DATA, CANCEL HANDLE, ABEND ABCODE 9999
  (Lines: 250, 857)
- **CICS READ RESP NOT NORMAL or NOTFND:** Set INPUT-ERROR, error flags, construct WS-FILE-ERROR-MESSAGE or specific not-found msg, re-send map
  (Lines: 752, 763)
- **Input validation fails (non-numeric, blank, zero):** Set INPUT-ERROR, specific flags, WS-RETURN-MSG, re-send map with error highlighting
  (Lines: 648, 666, 707)
- **Invalid AID/PFKEY:** Force to ENTER behavior, set PFK-INVALID
  (Lines: 291)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| HANDLE ABEND | ABEND-ROUTINE | Catch unhandled exceptions and route to abend handler | 250 |
| XCTL |  | Exit to prior program/menu on PF3, pass COMMAREA | 331 |
| RETURN |  | Return control to same transaction with updated COMMAREA | 402 |
| SEND MAP | CCARD-NEXT-MAP | Send screen with data, cursor, erase, freekb | 569 |
| RECEIVE MAP | LIT-THISMAP | Receive user input from screen into map | 597 |
| READ |  | Retrieve CARD-RECORD by cardnum RIDFLD | 742 |
| SEND TEXT |  | Debug/error plain text display and exit (not production) | 839 |
| ABEND |  | Terminate after abend display | 875 |

## Open Questions

- **Exact field layouts in copybooks like CVCRD01Y, CVCUS01Y, CSUSR01Y**
  - Context: Source includes COPY but not expanded content; fields inferred from usage
  - Suggestion: Provide expanded copybook source or preprocessor output
- **Details of YYYY-STORE-PFKEY from CSSTRPFY copybook**
  - Context: External PERFORM at 284 via COPY 'CSSTRPFY' at 855; handles AID to CCARD-AID-*
  - Suggestion: Include CSSTRPFY source
- **Invocation of 0-GETCARD-BYACCT paragraph**
  - Context: Defined at 779 but not PERFORMed in main flow; prepared for account-only search?
  - Suggestion: Confirm calling logic or conditional use
- **Full COMMAREA flow from callers like COCRDLIC or COMEN01C**
  - Context: Inferred from context checks but not direct source
  - Suggestion: Analyze calling programs

---
*Generated by War Rig WAR_RIG*