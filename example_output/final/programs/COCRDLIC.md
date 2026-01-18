# COCRDLIC

**File:** COCRDLIC.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 2
**Analyzed:** 2026-01-18 15:40:45.434265

## Purpose

CICS online program that browses and displays lists of credit card records from VSAM file CARDDAT, filtered optionally by account ID or card number entered on screen. Supports paging forward/backward with PF8/PF7, single record selection for view (S) or update (U), exit to menu with PF3, and XCTL transfers to detail view or update programs. Handles screen I/O via BMS map CCRDLIA and maintains paging context in COMMAREA.

**Business Context:** Credit card management: inquire and select cards for detail view or maintenance, with optional account/card filtering.
**Program Type:** ONLINE_CICS
**Citations:** Lines 4, 5, 6, 7, 298, 349, 359, 370, 402, 433, 450, 478, 486, 517, 545, 578

## Calling Context

**Called By:** [COMEN01C](./COMEN01C.md)
**Entry Points:** CCLI
**Linkage Section:** DFHCOMMAREA

## Inputs

### CCRDLIAI
- **Type:** CICS_MAP
- **Description:** BMS map input fields: CC-ACCT-ID (account filter), CC-CARD-NUM (card filter), CRDSELxI (select codes S/U for 7 rows)
- **Copybook:** [COCRDLI](../copybooks/COCRDLI.md)
- **Lines:** 963, 969, 970, 972, 973, 974, 975, 976, 977, 978

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Application COMMAREA with CARDDEMO-COMMAREA (context like CDEMO-ACCT-ID, CDEMO-FROM-PROGRAM) and WS-THIS-PROGCOMMAREA (paging keys WS-CA-LAST-CARDKEY, WS-CA-FIRST-CARDKEY, WS-CA-SCREEN-NUM)
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 315, 327, 328, 329, 330

### CARDDAT
- **Type:** FILE_VSAM
- **Description:** VSAM KSDS file of credit card records, key WS-CARD-RID (CARDNUM + ACCT-ID), read via browse for listing
- **Copybook:** [CVACT02Y](../copybooks/CVACT02Y.md)
- **Lines:** 1129, 1146, 1197, 1273, 1294, 1300, 1322

### EIB
- **Type:** PARAMETER
- **Description:** CICS execution interface block: EIBCALEN for COMMAREA length, EIBAID for PF key
- **Lines:** 315, 371, 372, 373, 374

## Outputs

### CCRDLIAO
- **Type:** CICS_MAP
- **Description:** BMS map output fields: titles, date/time, page no, screen rows ACCTNOxO/CRDNUMxO/CRDSTSxO/CRDSELxO, error/info messages ERRMSGO/INFOMSGO, next-prog/map
- **Copybook:** [COCRDLI](../copybooks/COCRDLI.md)
- **Lines:** 939, 643, 647, 648, 659, 664, 667, 924, 928

### WS-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated COMMAREA with context for reentry/paging and XCTL targets
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 615, 609, 610, 611, 612

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [COMEN01C](./COMEN01C.md) | CICS_XCTL | Exit to main menu on PF3 | 402 |
| [COCRDSLC](./COCRDSLC.md) | CICS_XCTL | Transfer to credit card detail view on 'S' select | 538 |
| [COCRDUPC](./COCRDUPC.md) | CICS_XCTL | Transfer to credit card update on 'U' select | 566 |

## Business Rules

### BR001: Account filter, if supplied, must be exactly 11-digit numeric
**Logic:** Check NOT NUMERIC or length via redefines, set error flag and message if invalid
**Conditions:** CC-ACCT-ID IS NOT NUMERIC
**Lines:** 1003, 1017, 1022

### BR002: Card filter, if supplied, must be exactly 16-digit numeric
**Logic:** Check NOT NUMERIC, set error flag and message if invalid
**Conditions:** CC-CARD-NUM IS NOT NUMERIC
**Lines:** 1036, 1052, 1058

### BR003: Exactly one row select allowed ('S' for view or 'U' for update)
**Logic:** Tally 'S'+'U' >1 sets error, invalid codes ('not S/U/blank') error
**Conditions:** I > 1, NOT SELECT-OK(I)
**Lines:** 1073, 1080, 1084, 1109

### BR004: Filter browse records to match account ID and/or card number if filters valid
**Logic:** Exclude if CARD-ACCT-ID != filter or CARD-NUM != filter
**Conditions:** FLG-ACCTFILTER-ISVALID AND CARD-ACCT-ID != CC-ACCT-ID, FLG-CARDFILTER-ISVALID AND CARD-NUM != CC-CARD-NUM
**Lines:** 1382, 1385, 1386, 1396, 1397

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVCRD01Y](../copybooks/CVCRD01Y.md) | WORKING_STORAGE | Defines CARD-RECORD layout (inferred from usage) | 221 |
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Application COMMAREA CARDDEMO-COMMAREA with context fields like CDEMO-ACCT-ID, CDEMO-FROM-PROGRAM | 227 |
| [COCRDLI](../copybooks/COCRDLI.md) | WORKING_STORAGE | BMS map definition for credit card list screen CCRDLIAI/O | 276 |
| [CVACT02Y](../copybooks/CVACT02Y.md) | WORKING_STORAGE | VSAM file record layout for CARDDAT | 290 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | IBM BMS symbolic map constants (e.g. DFHBMFSE) | 267 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | IBM AID constants (e.g. CCARD-AID-PFK03) | 268 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Current date fields WS-CURDATE-DATA | 279 |

## Data Flow

### Reads From
- **CARDDAT**: CARD-NUM, CARD-ACCT-ID, CARD-ACTIVE-STATUS
  (Lines: 1166, 1167, 1169, 1339, 1340, 1342)
- **CCRDLIAI**: ACCTSIDI, CARDSIDI, CRDSELxI
  (Lines: 969, 970, 972)

### Writes To
- **WS-SCREEN-ROWS**: WS-ROW-ACCTNO, WS-ROW-CARD-NUM, WS-ROW-CARD-STATUS
  (Lines: 1165, 1167, 1170)
- **CCRDLIAO**: ACCTNOxO, CRDNUMxO, CRDSTSxO, CRDSELxO
  (Lines: 683, 685, 686, 692)
- **WS-THIS-PROGCOMMAREA**: WS-CA-FIRST-CARDKEY, WS-CA-LAST-CARDKEY, WS-CA-SCREEN-NUM
  (Lines: 1175, 1176, 1194, 1195)

### Transformations
- **CARD-NUM** → **WS-ROW-CARD-NUM / CRDNUMxO**: Direct copy from file record to screen array row and map output
  (Lines: 1166, 685)
- **CARD-ACCT-ID** → **WS-ROW-ACCTNO / ACCTNOxO**: Direct copy from file record to screen array row and map output
  (Lines: 1167, 684)
- **CC-ACCT-ID** → **CDEMO-ACCT-ID**: Validated numeric move if input OK
  (Lines: 1027)

## Key Paragraphs

### 0000-MAIN
**Purpose:** Main control logic: initialize, handle COMMAREA, PF keys, input edits, dispatch to read/browse/send based on action
- Calls: YYYY-STORE-PFKEY, 2000-RECEIVE-MAP, 9000-READ-FORWARD, 1000-SEND-MAP, 9100-READ-BACKWARDS
- Lines: 298-603

### COMMON-RETURN
**Purpose:** Prepare and issue CICS RETURN with TRANSID and updated COMMAREA
- Lines: 604-620

### 1000-SEND-MAP
**Purpose:** Build and SEND map: init screen/array/attrs/message then send
- Calls: 1100-SCREEN-INIT, 1200-SCREEN-ARRAY-INIT, 1250-SETUP-ARRAY-ATTRIBS, 1300-SETUP-SCREEN-ATTRS, 1400-SETUP-MESSAGE, 1500-SEND-SCREEN
- Lines: 624-638

### 2000-RECEIVE-MAP
**Purpose:** RECEIVE map into CCRDLIAI, then receive screen and edit inputs
- Calls: 2100-RECEIVE-SCREEN, 2200-EDIT-INPUTS
- Lines: 951-958

### 9000-READ-FORWARD
**Purpose:** Forward browse: STARTBR GTEQ, loop READNEXT up to 7 records filtering, check next page, ENDBR
- Calls: 9500-FILTER-RECORDS
- Lines: 1123-1260

### 9100-READ-BACKWARDS
**Purpose:** Backward page: STARTBR GTEQ last key, READPREV 8 times to get prior 7 filtering, ENDBR
- Calls: 9500-FILTER-RECORDS
- Lines: 1264-1373

### 9500-FILTER-RECORDS
**Purpose:** Exclude record if doesn't match valid acct/card filters
- Lines: 1382-1408

## Error Handling

- **WS-RESP-CD NOT DFHRESP(NORMAL) or DUPREC on READNEXT/READPREV/STARTBR:** Set WS-ERROR-MSG with file error details, exit loop/browse
  (Lines: 1157, 1223, 1230, 1250, 1311, 1364)
- **INPUT-ERROR from edits (invalid numeric filters, multiple/invalid selects):** Set error message, protect select rows, resend map for correction
  (Lines: 419, 587, 1020, 1054, 1086, 1110)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| SEND MAP | CCRDLIA | Display credit card list screen with data, messages, cursor | 939 |
| RECEIVE MAP | CCRDLIA | Receive user inputs from screen filters and selects | 963 |
| STARTBR | CARDDAT | Initiate forward/backward browse GTEQ key | 1129 |
| READNEXT | CARDDAT | Fetch next record in forward browse | 1146 |
| READPREV | CARDDAT | Fetch previous record in backward browse | 1294 |
| ENDBR | CARDDAT | Terminate browse | 1258 |
| XCTL | COMEN01C | Transfer control to menu program | 402 |
| RETURN | CCLI | Return to CICS with TRANSID and COMMAREA for reentrancy | 615 |

## Open Questions

- **Exact fields in copybooks like CVACT02Y, COCOM01Y, COCRDLI?**
  - Context: Copybook sources unavailable; fields inferred from usage but not fully known
  - Suggestion: Analyze referenced copybooks
- **Admin user check for listing all cards?**
  - Context: Comment lines 5-6 mention admin user but no explicit code found (perhaps in CSUSR01Y or commarea USRTYP)
  - Suggestion: Check CSUSR01Y and user context logic
- **Full PF key mapping in YYYY-STORE-PFKEY?**
  - Context: COPY 'CSSTRPFY' at 1416; contents unknown
  - Suggestion: Analyze CSSTRPFY

---
*Generated by War Rig WAR_RIG*