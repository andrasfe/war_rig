# COUSR00C

**File:** COUSR00C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 2
**Analyzed:** 2026-01-18 15:50:08.435503

## Purpose

CICS online program that displays a paginated list of 10 users from the USRSEC VSAM file, handles user search via USRIDINI to set browse starting key (or beginning if blank), supports selection for Update (U) or Delete (D) via XCTL, forward/backward paging with PF8/PF7, and Enter/PF3 keys for navigation and return.

**Business Context:** User administration listing in CardDemo application, allowing search, browse, select for maintenance
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 5, 23, 98, 110, 122, 141, 218, 282, 336

## Calling Context

**Entry Points:** CU00
**Linkage Section:** DFHCOMMAREA

## Inputs

### USRSEC
- **Type:** FILE_VSAM
- **Description:** VSAM dataset containing user security records with fields like SEC-USR-ID, SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-TYPE
- **Copybook:** [CSUSR01Y](../copybooks/CSUSR01Y.md)
- **Lines:** 588, 621, 655

### COUSR0AI
- **Type:** CICS_MAP
- **Description:** Input map fields for user selection (SEL0001I-USRID10I), search (USRIDINI), page num (PAGENUMI) from USRLST screen
- **Copybook:** [COUSR00](../copybooks/COUSR00.md)
- **Lines:** 551

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** CARDDEMO-COMMAREA for passing program state including CDEMO-CU00-INFO (first/last user ID, page num, selection)
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 114

### EIBAID
- **Type:** CICS_COMMAREA
- **Description:** CICS attention key identifier (DFHENTER, DFHPF3, DFHPF7, DFHPF8)
- **Copybook:** [DFHAID](../copybooks/DFHAID.md)
- **Lines:** 122

## Outputs

### COUSR0AO
- **Type:** CICS_MAP
- **Description:** Output map fields for screen display including user list (USRID01I-UTYPE10I), header (titles, date/time), error message (ERRMSGO), page num (PAGENUMI), cleared search field (USRIDINO)
- **Copybook:** [COUSR00](../copybooks/COUSR00.md)
- **Lines:** 529, 537

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated commarea with page info, selected user for return or XCTL
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 141, 198, 208, 516

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [COUSR02C](./COUSR02C.md) | CICS_XCTL | Transfer control to user update program when 'U' selected | 197 |
| [COUSR03C](./COUSR03C.md) | CICS_XCTL | Transfer control to user delete program when 'D' selected | 207 |
| [COSGN00C](./COSGN00C.md) | CICS_XCTL | Return to signon screen on first entry or PF3 | 112 |
| [COADM01C](./COADM01C.md) | CICS_XCTL | Return to admin menu on PF3 alternative | 126 |

## Business Rules

### BR001: If user selects 'U' or 'u' in any SEL field with corresponding USRID, XCTL to COUSR02C for update
**Logic:** EVALUATE on SEL0001I-SEL0010I, check flag and value, then EVALUATE flag=='U/u'
**Conditions:** SEL000XI NOT = SPACES AND LOW-VALUES, CDEMO-CU00-USR-SEL-FLG = 'U' OR 'u'
**Lines:** 151, 190

### BR002: If user selects 'D' or 'd' in any SEL field with corresponding USRID, XCTL to COUSR03C for delete
**Logic:** Similar to BR001 but for 'D/d'
**Conditions:** SEL000XI NOT = SPACES AND LOW-VALUES, CDEMO-CU00-USR-SEL-FLG = 'D' OR 'd'
**Lines:** 151, 200

### BR003: Display error for invalid AID keypress
**Logic:** EVALUATE EIBAID WHEN OTHER, set error flag and message
**Conditions:** EIBAID NOT IN (DFHENTER, DFHPF3, DFHPF7, DFHPF8)
**Lines:** 132

### BR004: Page forward loads next 10 users starting from current position or low-values
**Logic:** STARTBR >= key, readnext up to 10, set first/last, check next-page possible
**Conditions:** PF8 or forward logic, USER-SEC-NOT-EOF
**Lines:** 282, 300

### BR005: Page backward loads previous 10 users ending at current position or high-values
**Logic:** STARTBR <= key, readprev up to 10 from end, adjust page num
**Conditions:** PF7, CDEMO-CU00-PAGE-NUM > 1
**Lines:** 336, 354

### BR006: Handle search input: if USRIDINI not spaces/low-values, set as SEC-USR-ID starting key for browse; else low-values (start from beginning); clear search field after use
**Logic:** In PROCESS-ENTER-KEY, conditional MOVE to SEC-USR-ID, then MOVE -1 to USRIDINL and SPACE to USRIDINO
**Conditions:** USRIDINI OF COUSR0AI = SPACES OR LOW-VALUES
**Lines:** 218, 224, 231

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Defines CARDDEMO-COMMAREA including CDEMO-CU00-INFO for paging and selection state | 66 |
| [COUSR00](../copybooks/COUSR00.md) | WORKING_STORAGE | Defines BMS map structures COUSR0AI (input) and COUSR0AO (output) for USRLST screen | 76 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Defines titles like CCDA-TITLE01/02 | 78 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Defines date/time working fields like WS-CURDATE-DATA | 79 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Defines messages like CCDA-MSG-INVALID-KEY | 80 |
| [CSUSR01Y](../copybooks/CSUSR01Y.md) | WORKING_STORAGE | Defines SEC-USER-DATA record layout for USRSEC file | 81 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | Defines EIBAID for attention key | 83 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | Defines BMS constants for mapset handling | 84 |

## Data Flow

### Reads From
- **USRSEC**: SEC-USR-ID, SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-TYPE
  (Lines: 621, 655)
- **COUSR0AI**: SEL0001I-SEL0010I, USRID01I-USRID10I, USRIDINI, PAGENUMI
  (Lines: 551)
- **CARDDEMO-COMMAREA**: CDEMO-CU00-USRID-FIRST, CDEMO-CU00-USRID-LAST, CDEMO-CU00-PAGE-NUM, CDEMO-CU00-USR-SEL-FLG, CDEMO-CU00-USR-SELECTED
  (Lines: 114)

### Writes To
- **COUSR0AO**: USRID01I-UTYPE10I, ERRMSGO, PAGENUMI, TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO, USRIDINO
  (Lines: 231, 384, 526, 567)
- **CARDDEMO-COMMAREA**: CDEMO-CU00-USRID-FIRST, CDEMO-CU00-USRID-LAST, CDEMO-CU00-PAGE-NUM, CDEMO-CU00-NEXT-PAGE-FLG, CDEMO-TO-PROGRAM, CDEMO-FROM-TRANID, CDEMO-FROM-PROGRAM, CDEMO-PGM-CONTEXT
  (Lines: 141, 310, 389, 435)

### Transformations
- **SEC-USR-ID** → **USRIDXXI OF COUSR0AI / CDEMO-CU00-USRID-FIRST-LAST**: Copy file user ID to indexed map field (1-10) and track first/last for paging
  (Lines: 388, 434)
- **SEC-USR-FNAME** → **FNAmEXXI OF COUSR0AI**: Copy file first name to indexed map field
  (Lines: 390)
- **SEC-USR-LNAME** → **LNAmEXXI OF COUSR0AI**: Copy file last name to indexed map field
  (Lines: 391)
- **SEC-USR-TYPE** → **UTYPEXXI OF COUSR0AI**: Copy file user type to indexed map field
  (Lines: 392)
- **SEL000XXI / USRIDXXI OF COUSR0AI** → **CDEMO-CU00-USR-SEL-FLG / CDEMO-CU00-USR-SELECTED**: Capture non-space/low selection flag and user ID from map to commarea
  (Lines: 152)
- **USRIDINI OF COUSR0AI** → **SEC-USR-ID**: Set browse starting key from search input if provided (not spaces/low-values), else low-values; used in STARTBR
  (Lines: 218)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Main control logic: initialize, handle first entry vs reentry, dispatch on EIBAID to process keys and send/receive map
- Calls: RETURN-TO-PREV-SCREEN, PROCESS-ENTER-KEY, SEND-USRLST-SCREEN, RECEIVE-USRLST-SCREEN, PROCESS-PF7-KEY, PROCESS-PF8-KEY
- Lines: 98-143

### PROCESS-ENTER-KEY
**Purpose:** Capture selected user from map SEL/USERID fields and process U/D to XCTL if valid; handle search USRIDINI to set SEC-USR-ID browse start key (low-values if blank), clear search fields (USRIDINL/USRIDINO), load first page forward
- Called by: MAIN-PARA
- Calls: PROCESS-PAGE-FORWARD
- Lines: 149-231

### PROCESS-PF7-KEY
**Purpose:** Handle previous page (backward) if not first page, else message
- Called by: MAIN-PARA
- Calls: PROCESS-PAGE-BACKWARD, SEND-USRLST-SCREEN
- Lines: 237-254

### PROCESS-PF8-KEY
**Purpose:** Handle next page (forward) if more pages, else message
- Called by: MAIN-PARA
- Calls: PROCESS-PAGE-FORWARD, SEND-USRLST-SCREEN
- Lines: 260-276

### PROCESS-PAGE-FORWARD
**Purpose:** Browse forward: startbr/readnext 10 records from SEC-USR-ID key, initialize/populate map, check next page possible
- Called by: PROCESS-ENTER-KEY, PROCESS-PF8-KEY
- Calls: STARTBR-USER-SEC-FILE, READNEXT-USER-SEC-FILE, INITIALIZE-USER-DATA, POPULATE-USER-DATA, ENDBR-USER-SEC-FILE, SEND-USRLST-SCREEN
- Lines: 282-330

### PROCESS-PAGE-BACKWARD
**Purpose:** Browse backward: startbr/readprev 10 records from SEC-USR-ID key, initialize/populate map, adjust page num
- Called by: PROCESS-PF7-KEY
- Calls: STARTBR-USER-SEC-FILE, READPREV-USER-SEC-FILE, INITIALIZE-USER-DATA, POPULATE-USER-DATA, ENDBR-USER-SEC-FILE, SEND-USRLST-SCREEN
- Lines: 336-378

### POPULATE-USER-DATA
**Purpose:** Move current SEC-USER-DATA fields to indexed map fields 1-10, track first/last
- Called by: PROCESS-PAGE-FORWARD, PROCESS-PAGE-BACKWARD
- Lines: 384-440

### INITIALIZE-USER-DATA
**Purpose:** Clear indexed map fields 1-10 to spaces
- Called by: PROCESS-PAGE-FORWARD, PROCESS-PAGE-BACKWARD
- Lines: 446-500

### RETURN-TO-PREV-SCREEN
**Purpose:** XCTL to previous program (COSGN00C or set CDEMO-TO-PROGRAM) with commarea
- Called by: MAIN-PARA
- Lines: 506-516

### SEND-USRLST-SCREEN
**Purpose:** Populate header/date/time, send map with/without ERASE
- Called by: MAIN-PARA, PROCESS-PF7-KEY, PROCESS-PF8-KEY, PROCESS-PAGE-FORWARD, PROCESS-PAGE-BACKWARD, STARTBR-USER-SEC-FILE, READNEXT-USER-SEC-FILE, READPREV-USER-SEC-FILE
- Calls: POPULATE-HEADER-INFO
- Lines: 522-543

### RECEIVE-USRLST-SCREEN
**Purpose:** Receive map into COUSR0AI with resp codes
- Called by: MAIN-PARA
- Lines: 549-556

### POPULATE-HEADER-INFO
**Purpose:** Set screen header with titles, tranid, pgmname, current date/time
- Called by: SEND-USRLST-SCREEN
- Lines: 562-581

### STARTBR-USER-SEC-FILE
**Purpose:** CICS STARTBR on USRSEC at SEC-USR-ID key, handle resp (NOTFND/OTHER errors)
- Called by: PROCESS-PAGE-FORWARD, PROCESS-PAGE-BACKWARD
- Calls: SEND-USRLST-SCREEN
- Lines: 586-614

### READNEXT-USER-SEC-FILE
**Purpose:** CICS READNEXT into SEC-USER-DATA, handle ENDFILE/OTHER errors
- Called by: PROCESS-PAGE-FORWARD
- Calls: SEND-USRLST-SCREEN
- Lines: 619-648

### READPREV-USER-SEC-FILE
**Purpose:** CICS READPREV into SEC-USER-DATA, handle ENDFILE/OTHER errors
- Called by: PROCESS-PAGE-BACKWARD
- Calls: SEND-USRLST-SCREEN
- Lines: 653-682

### ENDBR-USER-SEC-FILE
**Purpose:** CICS ENDBR on USRSEC
- Called by: PROCESS-PAGE-FORWARD, PROCESS-PAGE-BACKWARD
- Lines: 687-691

## Error Handling

- **WS-RESP-CD NOT = DFHRESP(NORMAL) in STARTBR:** NOTFND: set EOF, message 'top of page'; OTHER: set ERR-FLG, message 'Unable to lookup User', send screen
  (Lines: 597)
- **WS-RESP-CD = DFHRESP(ENDFILE) or NOT NORMAL in READNEXT:** Set EOF, message 'bottom of page', send screen
  (Lines: 631)
- **WS-RESP-CD = DFHRESP(ENDFILE) or NOT NORMAL in READPREV:** Set EOF, message 'top of page', send screen
  (Lines: 665)
- **Invalid EIBAID:** Set WS-ERR-FLG 'Y', invalid key message, send screen
  (Lines: 133)
- **Invalid user selection flag:** Set error message 'Invalid selection. Valid values are U and D'
  (Lines: 211)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| RETURN | TRANSID(WS-TRANID) COMMAREA(CARDDEMO-COMMAREA) | Return to CICS with new transaction and updated commarea | 141 |
| SEND | MAP('COUSR0A') MAPSET('COUSR00') FROM(COUSR0AO) | Send user list screen with cursor, optional ERASE | 529 |
| RECEIVE | MAP('COUSR0A') MAPSET('COUSR00') INTO(COUSR0AI) | Receive user input from screen | 551 |
| STARTBR | DATASET('USRSEC') RIDFLD(SEC-USR-ID) | Establish browse starting at key for forward/backward | 588 |
| READNEXT | DATASET('USRSEC') INTO(SEC-USER-DATA) RIDFLD(SEC-USR-ID) | Read next user record in browse | 621 |
| READPREV | DATASET('USRSEC') INTO(SEC-USER-DATA) RIDFLD(SEC-USR-ID) | Read previous user record in browse | 655 |
| ENDBR | DATASET('USRSEC') | End browse on USRSEC | 689 |
| XCTL | PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA) | Transfer control to selected maintenance program | 197 |

## Open Questions

- **Exact programs that call this via XCTL or LINK**
  - Context: Inferred from CDEMO-TO-PROGRAM usage like COSGN00C but no direct evidence in code
  - Suggestion: Analyze calling programs or CICS traces
- **Precise layout and all fields of copybooks like CSUSR01Y, COUSR00**
  - Context: Fields inferred from usage (e.g. SEC-USR-ID) but full structure unknown without copybook source
  - Suggestion: Provide copybook sources for next iteration
- **Business meaning of USER-TYPE values**
  - Context: Field copied but no validation or description in code
  - Suggestion: Review data dictionary or requirements

---
*Generated by War Rig WAR_RIG*