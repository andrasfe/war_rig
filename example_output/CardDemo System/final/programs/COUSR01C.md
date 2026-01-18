# COUSR01C

**File:** COUSR01C.cbl
**Type:** COBOL
**Status:** FinalStatus.VALHALLA
**Iterations:** 2
**Analyzed:** 2026-01-18 15:56:43.820666

## Purpose

CICS online program that presents a screen (COUSR1A) for adding new users to USRSEC file by capturing first name, last name, user ID, password, and type via map input, validates required fields are non-empty, writes VSAM record using user ID as key, handles duplicate keys and other errors with messages, supports PF3/PF4 keys for navigation/clear.

**Business Context:** CardDemo application user security administration: adds regular/admin users to USRSEC dataset.
**Program Type:** ONLINE_CICS
**Citations:** Lines 3, 5, 71, 115, 184, 238

## Calling Context

**Entry Points:** CU01
**Linkage Section:** DFHCOMMAREA

## Inputs

### COUSR1AI
- **Type:** CICS_MAP
- **Description:** Input map fields from screen: FNAMEI, LNAMEI, USERIDI, PASSWDI, USRTYPEI for new user details
- **Copybook:** [COUSR01](../copybooks/COUSR01.md)
- **Lines:** 203

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Program linkage commarea populated from previous program, used to pass CARDDEMO-COMMAREA context
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 82

### EIBAID
- **Type:** CICS_COMMAREA
- **Description:** CICS AID key from user input (ENTER, PF3, PF4, OTHER)
- **Copybook:** [DFHAID](../copybooks/DFHAID.md)
- **Lines:** 90

## Outputs

### COUSR1AO
- **Type:** CICS_MAP
- **Description:** Output map with populated header (titles, tranid, pgmname, date/time), error messages, and cursor positioning
- **Copybook:** [COUSR01](../copybooks/COUSR01.md)
- **Lines:** 190

### USRSEC
- **Type:** FILE_VSAM
- **Description:** New user security record written with fields SEC-USR-ID (key), SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-PWD, SEC-USR-TYPE
- **Copybook:** [CSUSR01Y](../copybooks/CSUSR01Y.md)
- **Lines:** 240

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated commarea passed back on RETURN or XCTL
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 107, 177

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | Transfer control to previous or next screen program (e.g., COSGN00C, COADM01C) | 176 |

## Business Rules

### BR001: First name must not be empty or low-values
**Logic:** EVALUATE checks FNAMEI OF COUSR1AI, sets error flag/message if invalid, repositions cursor
**Conditions:** FNAMEI OF COUSR1AI = SPACES OR LOW-VALUES
**Lines:** 118, 123

### BR002: Last name must not be empty or low-values
**Logic:** EVALUATE checks LNAMEI OF COUSR1AI, sets error flag/message if invalid, repositions cursor
**Conditions:** LNAMEI OF COUSR1AI = SPACES OR LOW-VALUES
**Lines:** 124, 129

### BR003: User ID must not be empty or low-values
**Logic:** EVALUATE checks USERIDI OF COUSR1AI, sets error flag/message if invalid, repositions cursor
**Conditions:** USERIDI OF COUSR1AI = SPACES OR LOW-VALUES
**Lines:** 130, 135

### BR004: Password must not be empty or low-values
**Logic:** EVALUATE checks PASSWDI OF COUSR1AI, sets error flag/message if invalid, repositions cursor
**Conditions:** PASSWDI OF COUSR1AI = SPACES OR LOW-VALUES
**Lines:** 136, 141

### BR005: User type must not be empty or low-values
**Logic:** EVALUATE checks USRTYPEI OF COUSR1AI, sets error flag/message if invalid, repositions cursor
**Conditions:** USRTYPEI OF COUSR1AI = SPACES OR LOW-VALUES
**Lines:** 142, 147

### BR006: Reject invalid AID keys
**Logic:** EVALUATE EIBAID WHEN OTHER sets error flag and invalid key message
**Conditions:** EIBAID NOT DFHENTER, DFHPF3, DFHPF4
**Lines:** 99, 102

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Defines CARDDEMO-COMMAREA including CDEMO-TO-PROGRAM, CDEMO-FROM-TRANID, CDEMO-FROM-PROGRAM, CDEMO-PGM-REENTER | 46 |
| [COUSR01](../copybooks/COUSR01.md) | WORKING_STORAGE | Defines BMS map input COUSR1AI and output COUSR1AO fields like FNAMEI, USERIDI, ERRMSGO | 48 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Defines title constants like CCDA-TITLE01, CCDA-TITLE02 | 50 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Defines date handling WS-CURDATE-DATA, WS-CURDATE-MM etc. | 51 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Defines message constants like CCDA-MSG-INVALID-KEY | 52 |
| [CSUSR01Y](../copybooks/CSUSR01Y.md) | WORKING_STORAGE | Defines SEC-USER-DATA record structure with SEC-USR-ID (key), SEC-USR-FNAME etc. | 53 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | Defines EIBAID and constants like DFHENTER, DFHPF3 | 55 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | Defines BMS attributes like DFHGREEN, ERASE, CURSOR | 56 |

## Data Flow

### Reads From
- **COUSR1AI**: FNAMEI, LNAMEI, USERIDI, PASSWDI, USRTYPEI
  (Lines: 203)
- **DFHCOMMAREA**: CDEMO-PGM-REENTER, CDEMO-TO-PROGRAM
  (Lines: 82)

### Writes To
- **USRSEC**: SEC-USR-ID, SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-PWD, SEC-USR-TYPE
  (Lines: 240)
- **COUSR1AO**: ERRMSGO, TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO, ERRMSGC
  (Lines: 190)

### Transformations
- **FNAMEI OF COUSR1AI** → **SEC-USR-FNAME**: Direct MOVE if all validations pass
  (Lines: 155)
- **LNAMEI OF COUSR1AI** → **SEC-USR-LNAME**: Direct MOVE if all validations pass
  (Lines: 156)
- **USERIDI OF COUSR1AI** → **SEC-USR-ID**: Direct MOVE if all validations pass
  (Lines: 154)
- **PASSWDI OF COUSR1AI** → **SEC-USR-PWD**: Direct MOVE if all validations pass
  (Lines: 157)
- **USRTYPEI OF COUSR1AI** → **SEC-USR-TYPE**: Direct MOVE if all validations pass
  (Lines: 158)
- **FUNCTION CURRENT-DATE** → **CURDATEO OF COUSR1AO**: Format MM-DD-YY from WS-CURDATE-DATA
  (Lines: 216, 228)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Main control logic: initialize, handle first entry vs reentry, receive map, process AID
- Calls: RETURN-TO-PREV-SCREEN, SEND-USRADD-SCREEN, RECEIVE-USRADD-SCREEN, PROCESS-ENTER-KEY, CLEAR-CURRENT-SCREEN
- Lines: 71-110

### PROCESS-ENTER-KEY
**Purpose:** Validate all input fields non-empty, move to SEC-USER-DATA if valid, perform write
- Called by: MAIN-PARA
- Calls: SEND-USRADD-SCREEN, WRITE-USER-SEC-FILE
- Lines: 115-160

### RETURN-TO-PREV-SCREEN
**Purpose:** Prepare commarea for XCTL to previous/next program
- Called by: MAIN-PARA
- Lines: 165-178

### SEND-USRADD-SCREEN
**Purpose:** Populate header, send map with message and ERASE/CURSOR
- Called by: MAIN-PARA, PROCESS-ENTER-KEY, CLEAR-CURRENT-SCREEN
- Calls: POPULATE-HEADER-INFO
- Lines: 184-196

### RECEIVE-USRADD-SCREEN
**Purpose:** Receive map into input area with RESP codes
- Called by: MAIN-PARA
- Lines: 201-209

### POPULATE-HEADER-INFO
**Purpose:** Set titles, tranid, pgmname, format current date/time
- Called by: SEND-USRADD-SCREEN
- Lines: 214-234

### WRITE-USER-SEC-FILE
**Purpose:** CICS WRITE VSAM record with RIDFLD, handle RESP outcomes
- Called by: PROCESS-ENTER-KEY
- Calls: INITIALIZE-ALL-FIELDS, SEND-USRADD-SCREEN
- Lines: 238-274

### CLEAR-CURRENT-SCREEN
**Purpose:** Clear by initializing fields and resending screen
- Called by: MAIN-PARA
- Calls: INITIALIZE-ALL-FIELDS, SEND-USRADD-SCREEN
- Lines: 279-283

### INITIALIZE-ALL-FIELDS
**Purpose:** Reset input fields to SPACES/LOW-VALUES and clear message
- Called by: CLEAR-CURRENT-SCREEN, WRITE-USER-SEC-FILE
- Lines: 287-295

## Error Handling

- **Input fields empty/low-values:** Set WS-ERR-FLG 'Y', load specific message to WS-MESSAGE, reposition cursor (-1 length), send screen
  (Lines: 118)
- **Invalid EIBAID:** Set WS-ERR-FLG 'Y', message CCDA-MSG-INVALID-KEY, send screen
  (Lines: 99)
- **CICS WRITE RESP = DUPKEY or DUPREC:** Set WS-ERR-FLG 'Y', message 'User ID already exist...', reposition USERIDL, send screen
  (Lines: 260)
- **CICS WRITE RESP OTHER/NORMAL:** NORMAL: success message, green attr, reinitialize; OTHER: error message, reposition FNAMEL, send screen
  (Lines: 250)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| RETURN | TRANSID(WS-TRANID) | Return to CICS with same transaction and updated COMMAREA | 107 |
| SEND | MAP('COUSR1A') MAPSET('COUSR01') | Send output map with ERASE and CURSOR | 190 |
| RECEIVE | MAP('COUSR1A') MAPSET('COUSR01') | Receive input map with RESP/RESP2 | 203 |
| WRITE | DATASET(WS-USRSEC-FILE) | Write VSAM record using RIDFLD(SEC-USR-ID) with RESP/RESP2 | 240 |
| XCTL | PROGRAM(CDEMO-TO-PROGRAM) | Transfer control to another program with COMMAREA | 176 |

## Open Questions

- **Exact structure and lengths of copybook fields (e.g., SEC-USR-ID length)**
  - Context: Copybook sources not provided; inferred from usage but not verifiable
  - Suggestion: Provide copybook sources for precise field definitions
- **Called_by programs**
  - Context: No JCL or PROC; inferred from XCTL usage but not explicit
  - Suggestion: Analyze calling programs like COSGN00C

---
*Generated by War Rig WAR_RIG*