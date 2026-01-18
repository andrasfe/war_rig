# COUSR02C

**File:** COUSR02C.cbl
**Type:** COBOL
**Status:** FinalStatus.VALHALLA
**Iterations:** 1
**Analyzed:** 2026-01-18 17:57:37.391653

## Purpose

CICS transaction CU02 program that displays the COUSR2A map for updating a user record from the USRSEC VSAM file. Handles loading existing user data on Enter, validates and updates fields (first name, last name, password, user type) on PF5/PF3, clears screen on PF4, and XCTLs back to prior program on PF3/PF12 or initial entry.

**Business Context:** CardDemo application user security maintenance, updating users selected from a list screen
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 5, 37, 82

## Calling Context

**Entry Points:** CU02
**Linkage Section:** DFHCOMMAREA

## Inputs

### COUSR2AI
- **Type:** CICS_MAP
- **Description:** Input fields from user update screen: USRIDINI, FNAMEI, LNAMEI, PASSWDI, USRTYPEI, length attributes
- **Copybook:** [COUSR02](../copybooks/COUSR02.md)
- **Lines:** 285, 288

### USRSEC
- **Type:** FILE_VSAM
- **Description:** VSAM dataset containing user records with SEC-USR-ID key and fields SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-PWD, SEC-USR-TYPE
- **Copybook:** [CSUSR01Y](../copybooks/CSUSR01Y.md)
- **Lines:** 322, 324

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Commarea from prior program with CDEMO-CU02-INFO including selected user ID (CDEMO-CU02-USR-SELECTED), from/to program/tran info
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 94, 50

## Outputs

### COUSR2AO
- **Type:** CICS_MAP
- **Description:** Output fields to user update screen: header (titles, date/time, tranid, pgmname), error message (ERRMSGO, ERRMSGC), data fields
- **Copybook:** [COUSR02](../copybooks/COUSR02.md)
- **Lines:** 272, 273

### USRSEC
- **Type:** FILE_VSAM
- **Description:** Updated user record with modified SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-PWD, SEC-USR-TYPE
- **Copybook:** [CSUSR01Y](../copybooks/CSUSR01Y.md)
- **Lines:** 360, 362

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated commarea passed to XCTL or RETURN with from/to program/tran context
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 256, 257

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | Transfer control to previous screen (e.g., COSGN00C signon, COADM01C admin, or caller) after update or cancel | 259 |

## Business Rules

### BR001: User ID must not be blank or low-values when loading or updating user
**Logic:** EVALUATE checks USRIDINI OF COUSR2AI, sets WS-ERR-FLG='Y', positions cursor with -1 length
**Conditions:** USRIDINI OF COUSR2AI = SPACES OR LOW-VALUES
**Lines:** 146, 180

### BR002: First name, last name, password, user type must not be blank or low-values for update
**Logic:** EVALUATE chain checks each field in UPDATE-USER-INFO, sets error flag and positions cursor
**Conditions:** FNAMEI/LNAMEI/PASSWDI/USRTYPEI OF COUSR2AI = SPACES OR LOW-VALUES
**Lines:** 186, 192, 198, 204

### BR003: Update only if at least one field differs from existing record
**Logic:** Compare input fields to SEC-USER-DATA fields, set USR-MODIFIED-YES if changed, REWRITE only if yes
**Conditions:** FNAMEI NOT = SEC-USR-FNAME, LNAMEI NOT = SEC-USR-LNAME, PASSWDI NOT = SEC-USR-PWD, USRTYPEI NOT = SEC-USR-TYPE
**Lines:** 219, 236

### BR004: On READ NOTFND, display 'User ID NOT found' error
**Logic:** EVALUATE WS-RESP-CD after READ, set error and reposition cursor
**Conditions:** WS-RESP-CD = DFHRESP(NOTFND)
**Lines:** 341

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Common commarea structure including CDEMO-CU02-INFO for transaction context (user selected, page, from/to programs) | 49 |
| [COUSR02](../copybooks/COUSR02.md) | WORKING_STORAGE | Defines BMS map input COUSR2AI and output COUSR2AO for user update screen fields and attributes | 60 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Common title constants (CCDA-TITLE01, CCDA-TITLE02) | 62 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Date/time working storage variables (WS-CURDATE-DATA, WS-CURTIME-* formats) | 63 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Message constants (CCDA-MSG-INVALID-KEY) | 64 |
| [CSUSR01Y](../copybooks/CSUSR01Y.md) | WORKING_STORAGE | User security record structure SEC-USER-DATA with SEC-USR-ID key and detail fields | 65 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | CICS AID keys (DFHENTER, DFHPF3, etc.) | 67 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | CICS BMS color/attr constants (DFHRED, DFHGREEN, DFHNEUTR) | 68 |

## Data Flow

### Reads From
- **USRSEC**: SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-PWD, SEC-USR-TYPE
  (Lines: 322, 166)
- **COUSR2AI**: USRIDINI, FNAMEI, LNAMEI, PASSWDI, USRTYPEI
  (Lines: 285, 157)
- **CARDDEMO-COMMAREA**: CDEMO-CU02-USR-SELECTED
  (Lines: 94, 99)

### Writes To
- **USRSEC**: SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-PWD, SEC-USR-TYPE
  (Lines: 360, 220)
- **COUSR2AO**: ERRMSGO, ERRMSGC, TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO
  (Lines: 270, 300)

### Transformations
- **USRIDINI OF COUSR2AI** → **SEC-USR-ID**: Direct move to key for READ
  (Lines: 162)
- **FNAMEI OF COUSR2AI** → **SEC-USR-FNAME**: Move if not equal to existing
  (Lines: 219)
- **SEC-USR-FNAME** → **FNAMEI OF COUSR2AI**: Move from file to input map on load
  (Lines: 167)
- **FUNCTION CURRENT-DATE** → **CURDATEO OF COUSR2AO**: Format MM-DD-YY from WS-CURDATE-DATA
  (Lines: 298, 309)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Entry point: initialize flags/messages, handle first entry (XCTL back), reentry (receive map, process AID)
- Calls: RETURN-TO-PREV-SCREEN, PROCESS-ENTER-KEY, SEND-USRUPD-SCREEN, RECEIVE-USRUPD-SCREEN, UPDATE-USER-INFO, CLEAR-CURRENT-SCREEN
- Lines: 82-137

### PROCESS-ENTER-KEY
**Purpose:** Validate/load user by ID from screen, populate screen fields from file
- Called by: MAIN-PARA
- Calls: SEND-USRUPD-SCREEN, READ-USER-SEC-FILE
- Lines: 143-173

### UPDATE-USER-INFO
**Purpose:** Validate screen fields, compare/load to file record, update if changed
- Called by: MAIN-PARA
- Calls: SEND-USRUPD-SCREEN, READ-USER-SEC-FILE, UPDATE-USER-SEC-FILE
- Lines: 177-245

### RETURN-TO-PREV-SCREEN
**Purpose:** Set from context and XCTL to prior program
- Called by: MAIN-PARA
- Lines: 250-261

### SEND-USRUPD-SCREEN
**Purpose:** Populate header, send map with erase/cursor
- Called by: MAIN-PARA, UPDATE-USER-INFO, READ-USER-SEC-FILE
- Calls: POPULATE-HEADER-INFO
- Lines: 266-278

### RECEIVE-USRUPD-SCREEN
**Purpose:** Receive map into input area with resp codes
- Called by: MAIN-PARA
- Lines: 283-291

### POPULATE-HEADER-INFO
**Purpose:** Set screen header with titles, tranid, pgmname, formatted date/time
- Called by: SEND-USRUPD-SCREEN
- Lines: 296-315

### READ-USER-SEC-FILE
**Purpose:** CICS READ UPDATE user record by RIDFLD, handle resp (normal/notfnd/other)
- Called by: PROCESS-ENTER-KEY, UPDATE-USER-INFO
- Calls: SEND-USRUPD-SCREEN
- Lines: 320-353

### UPDATE-USER-SEC-FILE
**Purpose:** CICS REWRITE user record, handle resp with success/error messages
- Called by: UPDATE-USER-INFO
- Calls: SEND-USRUPD-SCREEN
- Lines: 358-390

### CLEAR-CURRENT-SCREEN
**Purpose:** Initialize fields and resend screen
- Called by: MAIN-PARA
- Calls: INITIALIZE-ALL-FIELDS, SEND-USRUPD-SCREEN
- Lines: 395-399

### INITIALIZE-ALL-FIELDS
**Purpose:** Clear screen input fields and message
- Called by: CLEAR-CURRENT-SCREEN
- Lines: 403-411

## Error Handling

- **USRIDINI/FNAMEI/etc. = SPACES OR LOW-VALUES:** Set WS-ERR-FLG='Y', load error message to WS-MESSAGE, set field length -1 for cursor, PERFORM SEND-USRUPD-SCREEN
  (Lines: 147, 181)
- **WS-RESP-CD = DFHRESP(NOTFND) after READ:** Set WS-ERR-FLG='Y', message 'User ID NOT found...', cursor to USRIDINL
  (Lines: 341)
- **WS-RESP-CD NOT = DFHRESP(NORMAL) after READ/REWRITE:** DISPLAY resp/reason, set error flag, generic message, cursor position
  (Lines: 347, 384)
- **EIBAID = OTHER:** Set WS-ERR-FLG='Y', invalid key message
  (Lines: 128)
- **No modifications detected:** Message 'Please modify to update ...', set DFHRED color
  (Lines: 239)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| SEND | MAP('COUSR2A') MAPSET('COUSR02') FROM(COUSR2AO) | Display update screen with populated data, header, error message, erase and cursor positioning | 272 |
| RECEIVE | MAP('COUSR2A') MAPSET('COUSR02') INTO(COUSR2AI) | Receive edited user fields and AID from screen | 285 |
| READ | DATASET('USRSEC') INTO(SEC-USER-DATA) RIDFLD(SEC-USR-ID) | Read user record for display or update with UPDATE option | 322 |
| REWRITE | DATASET('USRSEC') FROM(SEC-USER-DATA) | Rewrite modified user record | 360 |
| XCTL | PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA) | Transfer to prior program preserving commarea | 259 |
| RETURN | TRANSID(WS-TRANID='CU02') COMMAREA(CARDDEMO-COMMAREA) | Restart own transaction after processing | 135 |

## Open Questions

- **Exact field layouts and additional fields in copybooks (e.g., full SEC-USER-DATA in CSUSR01Y, all map fields in COUSR02)**
  - Context: Copybooks not inlined; inferred from usage but unconfirmed
  - Suggestion: Analyze referenced copybook source files
- **Programs that call this via XCTL (e.g., user list screen pgm setting CDEMO-CU02-USR-SELECTED)**
  - Context: Inferred from commarea but not explicit
  - Suggestion: Trace callers of CU02 transaction or XCTL sources

---
*Generated by War Rig WAR_RIG*