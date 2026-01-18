# COMEN01C

**File:** COMEN01C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 2
**Analyzed:** 2026-01-18 16:16:05.236509

## Purpose

CICS COBOL program that presents a main menu screen to regular users in the CardDemo application, receives user option selection via ENTER key, validates the option based on user type and availability, and transfers control to the selected program using XCTL. Handles PF3 to return to signon screen and invalid inputs with error messages.

**Business Context:** CardDemo application main menu for regular users after signon
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 3, 5, 75, 93, 115, 147, 196

## Calling Context

**Called By:** [COSGN00C](./COSGN00C.md)
**Entry Points:** CM00
**Linkage Section:** DFHCOMMAREA

## Inputs

### COMEN1AI
- **Type:** CICS_MAP
- **Description:** Menu screen input map containing user-selected option in OPTIONI field
- **Copybook:** [COMEN01](../copybooks/COMEN01.md)
- **Lines:** 227, 92

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Program communication area passed from caller, moved to CARDDEMO-COMMAREA containing user context and menu data
- **Lines:** 86, 67

### EIBAID
- **Type:** OTHER
- **Description:** CICS attention key identifier (e.g., DFHENTER, DFHPF3) to determine user action
- **Copybook:** [DFHAID](../copybooks/DFHAID.md)
- **Lines:** 93

## Outputs

### COMEN1AO
- **Type:** CICS_MAP
- **Description:** Menu screen output map populated with header info, menu options, error messages, and selected option echo
- **Copybook:** [COMEN01](../copybooks/COMEN01.md)
- **Lines:** 215, 213

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated communication area with from-program, from-tranid, context, and user data for XCTL or RETURN
- **Lines:** 107, 156

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CDEMO-MENU-OPT-PGMNAME(WS-OPTION)](./CDEMO-MENU-OPT-PGMNAME(WS-OPTION).md) | CICS_XCTL | Transfer control to selected menu option program (e.g., COPAUS0C) after validation and inquire | 156 |
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | Return to signon screen program (COSGN00C) on PF3 or initial call | 202 |

## Business Rules

### BR001: Validate entered option is numeric, non-zero, and within menu option count
**Logic:** Trim spaces from input, inspect to replace spaces with zeros, move to numeric field, check NUMERIC, >0, <= count
**Conditions:** WS-OPTION IS NOT NUMERIC, WS-OPTION > CDEMO-MENU-OPT-COUNT, WS-OPTION = ZEROS
**Lines:** 117, 127

### BR002: Prevent regular users from selecting admin-only menu options
**Logic:** If user type is USER and option USRTYPE is 'A', set error flag and message
**Conditions:** CDEMO-USRTYP-USER, CDEMO-MENU-OPT-USRTYPE(WS-OPTION) = 'A'
**Lines:** 136

### BR003: Inquire program existence before XCTL for non-DUMMY options
**Logic:** EXEC CICS INQUIRE PROGRAM NOHANDLE, check EIBRESP NORMAL before XCTL
**Conditions:** CDEMO-MENU-OPT-PGMNAME(WS-OPTION) NOT STARTING 'DUMMY', CDEMO-MENU-OPT-PGMNAME(WS-OPTION) = 'COPAUS0C'
**Lines:** 147

### BR004: Handle DUMMY menu options with 'coming soon' message
**Logic:** If PGMNAME starts with 'DUMMY', set green message without XCTL
**Conditions:** CDEMO-MENU-OPT-PGMNAME(WS-OPTION)(1:5) = 'DUMMY'
**Lines:** 169

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Common variables including CARDDEMO-COMMAREA and WS-CURDATE-DATA | 50 |
| [COMEN02Y](../copybooks/COMEN02Y.md) | WORKING_STORAGE | Menu-related data structures | 51 |
| [COMEN01](../copybooks/COMEN01.md) | WORKING_STORAGE | BMS map structures COMEN1AI and COMEN1AO for menu screen | 53 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Title constants like CCDA-TITLE01 | 55 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Date and time formatting fields (WS-CURDATE-*, WS-CURTIME-*) | 56 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Message constants like CCDA-MSG-INVALID-KEY | 57 |
| [CSUSR01Y](../copybooks/CSUSR01Y.md) | WORKING_STORAGE | User security fields (e.g., SEC-USR-TYPE) | 58 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | CICS AID keys like DFHENTER, DFHPF3 | 60 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | BMS constants like DFHRED, DFHGREEN | 61 |

## Data Flow

### Reads From
- **COMEN1AI**: OPTIONI
  (Lines: 117)
- **CARDDEMO-COMMAREA**: CDEMO-PGM-REENTER, CDEMO-USRTYP-USER, CDEMO-MENU-OPT-COUNT, CDEMO-MENU-OPT-*
  (Lines: 86)
- **EIBAID**: DFHENTER, DFHPF3
  (Lines: 93)

### Writes To
- **COMEN1AO**: ERRMSGO, ERRMSGC, OPTIONO, OPTN001O-OPTN012O, TITLE01O, CURDATEO, CURTIMEO
  (Lines: 213, 125, 276, 242)
- **CARDDEMO-COMMAREA**: CDEMO-FROM-TRANID, CDEMO-FROM-PROGRAM, CDEMO-PGM-CONTEXT, CDEMO-TO-PROGRAM
  (Lines: 153)

### Transformations
- **OPTIONI OF COMEN1AI** → **WS-OPTION / OPTIONO OF COMEN1AO**: Trim trailing spaces, replace spaces with zeros, convert to numeric
  (Lines: 117)
- **FUNCTION CURRENT-DATE** → **CURDATEO / CURTIMEO OF COMEN1AO**: Format current date and time into MM-DD-YY and HH:MM:SS
  (Lines: 240)
- **CDEMO-MENU-OPT-NUM(WS-IDX) / CDEMO-MENU-OPT-NAME(WS-IDX)** → **OPTN00XO OF COMEN1AO**: String menu number, '. ', and name into option text fields via EVALUATE
  (Lines: 269)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Entry point: initialize, handle first call or reenter, receive map, process AID key
- Calls: RETURN-TO-SIGNON-SCREEN, SEND-MENU-SCREEN, RECEIVE-MENU-SCREEN, PROCESS-ENTER-KEY
- Lines: 75-111

### PROCESS-ENTER-KEY
**Purpose:** Parse and validate menu option, handle admin check, XCTL to selected program or error
- Called by: MAIN-PARA
- Calls: SEND-MENU-SCREEN
- Lines: 115-192

### RETURN-TO-SIGNON-SCREEN
**Purpose:** XCTL to signon program COSGN00C
- Called by: MAIN-PARA
- Lines: 196-204

### SEND-MENU-SCREEN
**Purpose:** Populate header and options, send map with ERASE
- Called by: MAIN-PARA, PROCESS-ENTER-KEY
- Calls: POPULATE-HEADER-INFO, BUILD-MENU-OPTIONS
- Lines: 208-221

### RECEIVE-MENU-SCREEN
**Purpose:** Receive map into COMEN1AI with RESP codes
- Called by: MAIN-PARA
- Lines: 225-234

### POPULATE-HEADER-INFO
**Purpose:** Set titles, tranid, pgmname, current date/time formatted
- Called by: SEND-MENU-SCREEN
- Lines: 238-258

### BUILD-MENU-OPTIONS
**Purpose:** Loop to build numbered option text into map fields OPTN001O-OPTN012O
- Called by: SEND-MENU-SCREEN
- Lines: 262-304

## Error Handling

- **Invalid EIBAID (OTHER):** Set error flag, load invalid key message, resend screen
  (Lines: 100)
- **Invalid option (non-numeric, zero, out of range):** Set error flag, custom message, resend screen
  (Lines: 127)
- **Admin option for regular user:** Set error flag, 'No access' message, resend screen
  (Lines: 136)
- **Program not found (EIBRESP NOT NORMAL after INQUIRE):** Set red message 'not installed', resend screen
  (Lines: 152)
- **CICS RECEIVE RESP/RESP2:** Captured into WS-RESP-CD and WS-REAS-CD but not checked or acted upon; program continues execution, which may lead to processing invalid or no input data if RECEIVE fails
  (Lines: 231)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| RETURN | TRANSID(WS-TRANID) | Return control with transaction CM00 and updated commarea | 107 |
| INQUIRE | PROGRAM(CDEMO-MENU-OPT-PGMNAME(WS-OPTION)) | Check if selected program is installed before XCTL | 148 |
| XCTL | PROGRAM(CDEMO-MENU-OPT-PGMNAME(WS-OPTION)) | Transfer to menu-selected program with commarea | 156 |
| SEND | MAP('COMEN1A') MAPSET('COMEN01') | Send menu screen with ERASE after population | 215 |
| RECEIVE | MAP('COMEN1A') MAPSET('COMEN01') | Receive user input from menu screen | 227 |

## Open Questions

- **Exact field layouts and values in copybook-defined structures like CARDDEMO-COMMAREA, CDEMO-MENU-OPT-***
  - Context: Copybook source not provided, inferred from usage but details UNKNOWN
  - Suggestion: Analyze copybooks COCOM01Y, COMEN02Y, COMEN01
- **Full list of menu options and their programs/USRTYPE from CDEMO-MENU-OPT-***
  - Context: Defined in copybooks, usage shows dynamic via WS-IDX up to CDEMO-MENU-OPT-COUNT
  - Suggestion: Examine copybook data initialization
- **Handling of WS-RESP-CD and WS-REAS-CD from RECEIVE**
  - Context: Captured but no explicit use or check visible
  - Suggestion: Check if used elsewhere or in copybooks

---
*Generated by War Rig WAR_RIG*