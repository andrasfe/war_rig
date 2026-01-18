# COUSR03C

**File:** COUSR03C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 1
**Analyzed:** 2026-01-18 18:09:10.364325

## Purpose

CICS online program that displays a delete confirmation screen for a selected user ID from the USRSEC VSAM file, reads user details, populates the screen with name and type, and deletes the record upon PF5 key press. Handles navigation keys like PF3/PF12 to return to previous screens (COSGN00C or COADM01C) and PF4 to clear. Processes input from map and commarea passed from prior program.

**Business Context:** User administration in CardDemo application: delete user security records
**Program Type:** ONLINE_CICS
**Citations:** Lines 3, 5, 37, 82, 213, 305

## Calling Context

**Called By:** [COUSR03](./COUSR03.md)
**Entry Points:** CU03
**Linkage Section:** DFHCOMMAREA, LK-COMMAREA

## Inputs

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Passed commarea containing prior program context, selected user ID from list screen (CDEMO-CU03-USR-SELECTED), page info, and navigation flags
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 49, 74, 94, 99

### COUSR3AI
- **Type:** CICS_MAP
- **Description:** Input map fields: user ID (USRIDINI), length (USRIDINL), first/last name, type from screen receive
- **Copybook:** [COUSR03](../copybooks/COUSR03.md)
- **Lines:** 60, 232

### USRSEC
- **Type:** FILE_VSAM
- **Description:** VSAM dataset read by user ID (SEC-USR-ID) to retrieve/populate user details (SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-TYPE)
- **Copybook:** [CSUSR01Y](../copybooks/CSUSR01Y.md)
- **Lines:** 38, 65, 269

## Outputs

### COUSR3AO
- **Type:** CICS_MAP
- **Description:** Output map for delete screen: header (titles, date/time, tranid, pgmname), error message (ERRMSGO), user details (FNAMEI, LNAMEI, USRTYPEI), cursor position
- **Copybook:** [COUSR03](../copybooks/COUSR03.md)
- **Lines:** 60, 219

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated commarea with current context passed to next program via XCTL or RETURN TRANSID
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 49, 136, 207

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [COSGN00C](./COSGN00C.md) | CICS_XCTL | Return to signon screen when no prior program or on PF12 | 92 |
| [COADM01C](./COADM01C.md) | CICS_XCTL | Return to admin menu on PF3/PF12 when no prior program specified | 113 |
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | Dynamic XCTL to previous program (set from CDEMO-FROM-PROGRAM) on PF3 | 206 |

## Business Rules

### BR001: User ID must not be empty or low-values before read or delete
**Logic:** EVALUATE TRUE sets error flag and message if empty
**Conditions:** USRIDINI OF COUSR3AI = SPACES OR LOW-VALUES
**Lines:** 145, 177

### BR002: On successful VSAM read, populate screen with user name/type and prompt for PF5 delete
**Logic:** If DFHRESP(NORMAL), move fields to input map and send screen
**Conditions:** WS-RESP-CD = DFHRESP(NORMAL)
**Lines:** 280, 165

### BR003: Delete VSAM record only on PF5 after read confirms existence
**Logic:** Perform READ then DELETE, handle responses with messages
**Conditions:** EIBAID = DFHPF5, NOT ERR-FLG-ON
**Lines:** 122, 188, 191

### BR004: Invalid AID keys set error and redisplay screen
**Logic:** WHEN OTHER in EVALUATE EIBAID sets error flag
**Conditions:** EIBAID NOT in (DFHENTER, DFHPF3, DFHPF4, DFHPF5, DFHPF12)
**Lines:** 127

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Common commarea structure including CDEMO-CU03-INFO with user selection and navigation data | 49 |
| [COUSR03](../copybooks/COUSR03.md) | WORKING_STORAGE | BMS map structures COUSR3AI (input) and COUSR3AO (output) for delete screen fields | 60 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Titles and literals (e.g. CCDA-TITLE01) | 62 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Date formatting fields (WS-CURDATE-DATA, WS-CURDATE-MM-DD-YY) | 63 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Message constants (e.g. CCDA-MSG-INVALID-KEY) | 64 |
| [CSUSR01Y](../copybooks/CSUSR01Y.md) | WORKING_STORAGE | VSAM user record structure SEC-USER-DATA with SEC-USR-ID, SEC-USR-FNAME, etc. | 65 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | CICS AID keys (EIBAID, DFHENTER, DFHPF3, etc.) | 67 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | BMS constants (DFHNEUTR, DFHGREEN, etc.) | 68 |

## Data Flow

### Reads From
- **USRSEC**: SEC-USR-ID, SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-TYPE
  (Lines: 160, 269)
- **COUSR3AI**: USRIDINI, USRIDINL, FNAMEI, LNAMEI, USRTYPEI
  (Lines: 102, 144, 232)
- **CARDDEMO-COMMAREA**: CDEMO-CU03-USR-SELECTED, CDEMO-FROM-PROGRAM, CDEMO-PGM-REENTER
  (Lines: 94, 99)

### Writes To
- **COUSR3AO**: ERRMSGO, TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO, ERRMSGC
  (Lines: 217, 247, 219)
- **USRSEC**: all fields
  (Lines: 307)

### Transformations
- **FUNCTION CURRENT-DATE** → **CURDATEO OF COUSR3AO**: Format current date as MM-DD-YY from WS-CURDATE-DATA
  (Lines: 245, 256)
- **SEC-USR-FNAME** → **FNAMEI OF COUSR3AI**: Move read VSAM first name to map input field for display
  (Lines: 165)
- **SEC-USR-ID** → **WS-MESSAGE**: String 'User ' + SEC-USR-ID + ' has been deleted ...' on successful delete
  (Lines: 318)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Entry point: initialize flags, handle first entry or reentry, process AID keys, perform actions, return with TRANSID
- Calls: RETURN-TO-PREV-SCREEN, PROCESS-ENTER-KEY, SEND-USRDEL-SCREEN, RECEIVE-USRDEL-SCREEN, CLEAR-CURRENT-SCREEN, DELETE-USER-INFO
- Lines: 82-138

### PROCESS-ENTER-KEY
**Purpose:** Validate user ID input, read VSAM if valid, populate screen with details
- Called by: MAIN-PARA
- Calls: SEND-USRDEL-SCREEN, READ-USER-SEC-FILE
- Lines: 142-171

### DELETE-USER-INFO
**Purpose:** Validate user ID, read VSAM, then delete record
- Called by: MAIN-PARA
- Calls: SEND-USRDEL-SCREEN, READ-USER-SEC-FILE, DELETE-USER-SEC-FILE
- Lines: 174-194

### RETURN-TO-PREV-SCREEN
**Purpose:** Set XCTL target to prior or default program, XCTL with commarea
- Called by: MAIN-PARA
- Lines: 197-209

### SEND-USRDEL-SCREEN
**Purpose:** Populate header, move message, send map COUSR3A ERASE CURSOR
- Called by: MAIN-PARA, PROCESS-ENTER-KEY, DELETE-USER-INFO, READ-USER-SEC-FILE, DELETE-USER-SEC-FILE
- Calls: POPULATE-HEADER-INFO
- Lines: 213-227

### RECEIVE-USRDEL-SCREEN
**Purpose:** Receive map COUSR3A into COUSR3AI with RESP codes
- Called by: MAIN-PARA
- Lines: 230-240

### POPULATE-HEADER-INFO
**Purpose:** Format current date/time, move titles/tranid/pgmname to map header
- Called by: SEND-USRDEL-SCREEN
- Lines: 243-264

### READ-USER-SEC-FILE
**Purpose:** CICS READ VSAM USRSEC UPDATE with RIDFLD, handle RESP/NOTFND/OTHER
- Called by: PROCESS-ENTER-KEY, DELETE-USER-INFO
- Calls: SEND-USRDEL-SCREEN
- Lines: 267-301

### DELETE-USER-SEC-FILE
**Purpose:** CICS DELETE VSAM USRSEC, handle RESP/NORMAL/NOTFND/OTHER with messages
- Called by: DELETE-USER-INFO
- Calls: INITIALIZE-ALL-FIELDS, SEND-USRDEL-SCREEN
- Lines: 305-338

### CLEAR-CURRENT-SCREEN
**Purpose:** Clear fields and resend screen
- Called by: MAIN-PARA
- Calls: INITIALIZE-ALL-FIELDS, SEND-USRDEL-SCREEN
- Lines: 341-346

### INITIALIZE-ALL-FIELDS
**Purpose:** Set input lengths to -1, spaces to user fields and message
- Called by: DELETE-USER-SEC-FILE, CLEAR-CURRENT-SCREEN
- Lines: 349-357

## Error Handling

- **USRIDINI empty or low-values:** Set WS-ERR-FLG 'Y', message 'User ID can NOT be empty...', resend screen
  (Lines: 146, 178)
- **VSAM READ RESP NOTFND:** Set error flag, message 'User ID NOT found...', resend screen
  (Lines: 288)
- **VSAM READ/DELETE RESP OTHER:** DISPLAY RESP/REAS, set error flag, message 'Unable to lookup/update User...', resend screen
  (Lines: 294, 330)
- **Invalid EIBAID:** Set WS-ERR-FLG 'Y', move CCDA-MSG-INVALID-KEY to message, resend screen
  (Lines: 127)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| RETURN | TRANSID(WS-TRANID) | Return to CICS with same transaction ID and commarea | 134 |
| SEND | MAP('COUSR3A') MAPSET('COUSR03') | Send delete screen map ERASE CURSOR from COUSR3AO | 219 |
| RECEIVE | MAP('COUSR3A') MAPSET('COUSR03') | Receive map into COUSR3AI with RESP codes | 232 |
| READ | DATASET(WS-USRSEC-FILE) | Read VSAM record by RIDFLD(SEC-USR-ID) UPDATE | 269 |
| DELETE | DATASET(WS-USRSEC-FILE) | Delete VSAM record | 307 |
| XCTL | PROGRAM(CDEMO-TO-PROGRAM) | Transfer control to previous program with commarea | 206 |

## Open Questions

- **Exact structure of SEC-USER-DATA and field lengths**
  - Context: Inferred from CSUSR01Y copybook usage but not visible in source
  - Suggestion: Analyze CSUSR01Y copybook
- **Full list of programs calling this (beyond COUSR03 inference)**
  - Context: Inferred from commarea fields like CDEMO-CU03-USR-SELECTED
  - Suggestion: Trace callers via preprocessor or JCL

---
*Generated by War Rig WAR_RIG*