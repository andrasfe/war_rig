# COSGN00C

**File:** COSGN00C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 1
**Analyzed:** 2026-01-18 16:03:06.465226

## Purpose

CICS COBOL program that displays a signon screen for the CardDemo application, receives user ID and password via map COSGN0A, validates credentials against VSAM file USRSEC, populates commarea on success, and XCTLs to admin (COADM01C) or main menu (COMEN01C) program based on user type. On errors or invalid keys, redisplays screen with messages. Handles PF3 for exit message.

**Business Context:** User authentication and initial screen for CardDemo application
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 5, 73, 80, 108, 209, 221

## Calling Context

**Entry Points:** CC00
**Linkage Section:** DFHCOMMAREA

## Inputs

### COSGN0A
- **Type:** CICS_MAP
- **Description:** Signon screen map containing USERIDI and PASSWDI fields from user input
- **Copybook:** [COSGN00](../copybooks/COSGN00.md)
- **Lines:** 110, 118, 123

### USRSEC
- **Type:** FILE_VSAM
- **Description:** User security VSAM file read by user ID key to validate password and retrieve user type
- **Copybook:** [CSUSR01Y](../copybooks/CSUSR01Y.md)
- **Lines:** 211

### EIBAID
- **Type:** PARAMETER
- **Description:** CICS AID key from keyboard (ENTER, PF3, etc.) to determine action
- **Copybook:** [DFHAID](../copybooks/DFHAID.md)
- **Lines:** 85

## Outputs

### COSGN0A
- **Type:** CICS_MAP
- **Description:** Signon screen map populated with header info, error messages, and cursor positioning for redisplay
- **Copybook:** [COSGN00](../copybooks/COSGN00.md)
- **Lines:** 151

### WS-MESSAGE
- **Type:** REPORT
- **Description:** Plain text error or exit message sent via SEND TEXT with FREEKB before RETURN
- **Lines:** 164

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Populated commarea passed to XCTL programs and RETURN TRANSID
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 98, 229, 232, 238

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [COADM01C](./COADM01C.md) | CICS_XCTL | Transfer control to admin menu for users with admin type | 231 |
| [COMEN01C](./COMEN01C.md) | CICS_XCTL | Transfer control to main menu for non-admin users | 236 |

## Business Rules

### BR001: On first invocation (EIBCALEN=0), initialize and send blank signon screen
**Logic:** MOVE LOW-VALUES TO COSGN0AO and set USERIDL=-1 then PERFORM SEND-SIGNON-SCREEN
**Conditions:** IF EIBCALEN = 0
**Lines:** 80, 81, 83

### BR002: Validate USERIDI and PASSWDI not SPACES or LOW-VALUES on ENTER key
**Logic:** EVALUATE TRUE checking each field, set error flag/message and resend screen with cursor
**Conditions:** WHEN USERIDI OF COSGN0AI = SPACES OR LOW-VALUES, WHEN PASSWDI OF COSGN0AI = SPACES OR LOW-VALUES
**Lines:** 117, 123

### BR003: Convert user ID and password to uppercase before validation
**Logic:** MOVE FUNCTION UPPER-CASE to WS-USER-ID/WS-USER-PWD and CDEMO-USER-ID
**Lines:** 132, 135

### BR004: On successful read (RESP=0) and password match, populate commarea and XCTL based on SEC-USR-TYPE
**Logic:** Set CDEMO fields then IF CDEMO-USRTYP-ADMIN XCTL COADM01C ELSE COMEN01C
**Conditions:** WHEN 0, IF SEC-USR-PWD = WS-USER-PWD, IF CDEMO-USRTYP-ADMIN
**Lines:** 221, 223, 231

### BR005: Handle invalid AID keys by setting error and resending screen
**Logic:** WHEN OTHER MOVE 'Y' TO WS-ERR-FLG and invalid key message
**Conditions:** WHEN OTHER
**Lines:** 91, 92

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Defines CARDDEMO-COMMAREA and related CDEMO- fields for commarea | 48 |
| [COSGN00](../copybooks/COSGN00.md) | WORKING_STORAGE | Defines signon map structures COSGN0AI and COSGN0AO with fields like USERIDI, PASSWDI, ERRMSGO | 50 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Defines title literals like CCDA-TITLE01, CCDA-TITLE02 | 52 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Defines date/time working fields like WS-CURDATE-DATA | 53 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Defines message literals like CCDA-MSG-INVALID-KEY | 54 |
| [CSUSR01Y](../copybooks/CSUSR01Y.md) | WORKING_STORAGE | Defines SEC-USER-DATA with SEC-USR-PWD, SEC-USR-TYPE | 55 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | Defines EIBAID and AID constants like DFHENTER, DFHPF3 | 57 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | Defines BMS map control fields like USERIDL, PASSWDL | 58 |

## Data Flow

### Reads From
- **COSGN0AI**: USERIDI, PASSWDI
  (Lines: 118, 123)
- **USRSEC**: SEC-USR-PWD, SEC-USR-TYPE
  (Lines: 223, 227)
- **EIBAID**: DFHENTER, DFHPF3
  (Lines: 86)

### Writes To
- **COSGN0AO**: ERRMSGO, TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO, APPLIDO, SYSIDO, USERIDL, PASSWDL
  (Lines: 149, 181, 198)
- **CARDDEMO-COMMAREA**: CDEMO-FROM-TRANID, CDEMO-FROM-PROGRAM, CDEMO-USER-ID, CDEMO-USER-TYPE, CDEMO-PGM-CONTEXT
  (Lines: 224)

### Transformations
- **USERIDI OF COSGN0AI** → **WS-USER-ID**: Convert to UPPER-CASE
  (Lines: 132)
- **PASSWDI OF COSGN0AI** → **WS-USER-PWD**: Convert to UPPER-CASE
  (Lines: 135)
- **FUNCTION CURRENT-DATE** → **CURDATEO OF COSGN0AO**: Format as MM-DD-YY
  (Lines: 179, 190)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Main entry point: initialize, handle EIBCALEN=0 or EIBAID to dispatch to process or send screens
- Calls: SEND-SIGNON-SCREEN, PROCESS-ENTER-KEY, SEND-PLAIN-TEXT
- Lines: 73-102

### PROCESS-ENTER-KEY
**Purpose:** RECEIVE map, validate inputs, uppercase credentials, read security file if valid
- Called by: MAIN-PARA
- Calls: SEND-SIGNON-SCREEN, READ-USER-SEC-FILE
- Lines: 108-141

### SEND-SIGNON-SCREEN
**Purpose:** Populate header and message then SEND MAP with ERASE and CURSOR
- Called by: MAIN-PARA, PROCESS-ENTER-KEY, READ-USER-SEC-FILE
- Calls: POPULATE-HEADER-INFO
- Lines: 145-157

### SEND-PLAIN-TEXT
**Purpose:** SEND TEXT message with ERASE and FREEKB then RETURN
- Called by: MAIN-PARA
- Lines: 162-172

### POPULATE-HEADER-INFO
**Purpose:** Set titles, tranid, pgmname, formatted date/time, applid, sysid in map
- Called by: SEND-SIGNON-SCREEN
- Lines: 177-204

### READ-USER-SEC-FILE
**Purpose:** CICS READ VSAM USRSEC by user ID, evaluate RESP to validate pwd/user and XCTL or error
- Called by: PROCESS-ENTER-KEY
- Calls: SEND-SIGNON-SCREEN
- Lines: 209-257

## Error Handling

- **USERIDI or PASSWDI = SPACES OR LOW-VALUES:** Set WS-ERR-FLG='Y', specific message, reposition cursor (-1), PERFORM SEND-SIGNON-SCREEN
  (Lines: 118, 123)
- **WS-RESP-CD = 0 AND SEC-USR-PWD != WS-USER-PWD:** Wrong password message, reposition PASSWDL=-1, PERFORM SEND-SIGNON-SCREEN
  (Lines: 242)
- **WS-RESP-CD = 13:** User not found message, set WS-ERR-FLG='Y', reposition USERIDL=-1, PERFORM SEND-SIGNON-SCREEN
  (Lines: 247)
- **WS-RESP-CD OTHER:** Unable to verify message, set WS-ERR-FLG='Y', reposition USERIDL=-1, PERFORM SEND-SIGNON-SCREEN
  (Lines: 252)
- **EIBAID = OTHER:** Invalid key message, set WS-ERR-FLG='Y', PERFORM SEND-SIGNON-SCREEN
  (Lines: 92)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| RETURN | TRANSID(WS-TRANID) | Return to CICS with next transaction CC00 and commarea | 98 |
| RECEIVE | MAP('COSGN0A') MAPSET('COSGN00') | Receive user input from signon map | 110 |
| SEND | MAP('COSGN0A') MAPSET('COSGN00') | Send signon map with ERASE and CURSOR for display or redisplay | 151 |
| SEND TEXT |  | Send plain text exit message with ERASE and FREEKB | 164 |
| READ | DATASET(WS-USRSEC-FILE) | Read VSAM USRSEC file by RIDFLD user ID | 211 |
| XCTL | PROGRAM('COADM01C') | Transfer control to admin program with commarea | 231 |

## Open Questions

- **Exact layout and fields of CARDDEMO-COMMAREA and CDEMO- fields**
  - Context: Defined in copybook COCOM01Y not provided in source
  - Suggestion: Analyze COCOM01Y copybook
- **Definition of SEC-USER-DATA fields like SEC-USR-TYPE constants (CDEMO-USRTYP-ADMIN)**
  - Context: Defined in CSUSR01Y copybook not provided
  - Suggestion: Analyze CSUSR01Y copybook
- **Programs that XCTL or LINK to COSGN00C**
  - Context: No static analysis info on callers
  - Suggestion: Trace transaction CC00 or JCL/CICS definitions

---
*Generated by War Rig WAR_RIG*