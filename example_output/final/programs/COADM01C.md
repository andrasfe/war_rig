# COADM01C

**File:** COADM01C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 2
**Analyzed:** 2026-01-18 17:20:00.572072

## Purpose

COADM01C is a pseudoconversational CICS program that presents an admin menu screen (COADM1A) listing up to 10 configurable admin options from the CDEMO-ADMIN-OPT table. On first invocation (EIBCALEN=0) or PF3, it XCTLs back to signon program COSGN00C; otherwise receives user input, processes ENTER key by validating numeric option (1 to CDEMO-ADMIN-OPT-COUNT), and XCTLs to the selected program if not starting with 'DUMMY'; invalid inputs or uninstalled programs display error messages and redisplay the menu.

**Business Context:** Provides menu-driven access to administrative functions in the CardDemo application for users with admin privileges.
**Program Type:** ONLINE_CICS
**Citations:** Lines 5, 37, 75, 86, 98, 100, 141, 146

## Calling Context

**Entry Points:** CA00
**Linkage Section:** DFHCOMMAREA

## Inputs

### COADM1AI
- **Type:** CICS_MAP
- **Description:** Input fields from BMS map COADM1A, primarily OPTIONI containing user-selected menu option number
- **Copybook:** [COADM01](../copybooks/COADM01.md)
- **Lines:** 194, 121

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Passed from prior invocation or caller, used to check CDEMO-PGM-REENTER flag and pass context to XCTL targets
- **Copybook:** [COADM01](../copybooks/COADM01.md)
- **Lines:** 90, 91

### EIBAID
- **Type:** OTHER
- **Description:** CICS attention identifier key for ENTER (DFHENTER) or PF3 (DFHPF3) processing
- **Copybook:** [DFHAID](../copybooks/DFHAID.md)
- **Lines:** 97

### EIBCALEN
- **Type:** OTHER
- **Description:** Commarea length to distinguish first invocation from subsequent pseudoconversational receives
- **Lines:** 86

## Outputs

### COADM1AO
- **Type:** CICS_MAP
- **Description:** Output fields for BMS map COADM1A including header (titles, tran/program/date/time), dynamically built menu options (OPTN001O-OPTN010O), selected option echo (OPTIONO), and error message (ERRMSGO)
- **Copybook:** [COADM01](../copybooks/COADM01.md)
- **Lines:** 182

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated commarea returned via CICS RETURN for next transaction start, including from-tran/program and context for XCTL chain
- **Copybook:** [COADM01](../copybooks/COADM01.md)
- **Lines:** 112

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION)](./CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION).md) | CICS_XCTL | Transfer control to user-selected admin option program (if not 'DUMMY') with commarea | 146 |
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | Return control to signon program (COSGN00C) on PF3 or initial call | 169 |
| [COSGN00C](./COSGN00C.md) | CICS_XCTL | Default XCTL target for signon screen on first invocation or PF3 exit | 88 |

## Business Rules

### BR001: Validate selected menu option is numeric, greater than zero, and within valid count
**Logic:** Trim trailing spaces from OPTIONI, INSPECT REPLACING spaces with zeros, move to WS-OPTION, check NUMERIC, > CDEMO-ADMIN-OPT-COUNT, = ZEROS
**Conditions:** WS-OPTION IS NOT NUMERIC, WS-OPTION > CDEMO-ADMIN-OPT-COUNT, WS-OPTION = ZEROS
**Lines:** 121, 131

### BR002: Only XCTL to admin program if its name does not start with 'DUMMY'
**Logic:** IF CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION)(1:5) NOT = 'DUMMY' then XCTL
**Conditions:** CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION)(1:5) NOT = 'DUMMY'
**Lines:** 141

### BR003: On PF3 attention key, XCTL back to signon screen
**Logic:** EVALUATE EIBAID WHEN DFHPF3: MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM, PERFORM RETURN-TO-SIGNON-SCREEN
**Conditions:** EIBAID = DFHPF3
**Lines:** 100, 101

### BR004: On first invocation (EIBCALEN=0), XCTL to signon screen
**Logic:** MOVE 'COSGN00C' TO CDEMO-FROM-PROGRAM, PERFORM RETURN-TO-SIGNON-SCREEN
**Conditions:** EIBCALEN = 0
**Lines:** 86, 88

### BR005: On invalid attention key (neither ENTER nor PF3), display invalid key error and redisplay menu
**Logic:** EVALUATE EIBAID WHEN OTHER: MOVE 'Y' TO WS-ERR-FLG, MOVE CCDA-MSG-INVALID-KEY TO WS-MESSAGE, PERFORM SEND-MENU-SCREEN
**Conditions:** EIBAID = OTHER
**Lines:** 103, 104

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Common variables or constants (usage not visible in source; possibly WS-VARIABLES related) | 50 |
| [COADM02Y](../copybooks/COADM02Y.md) | WORKING_STORAGE | Admin menu configuration or additional structures (possibly defines CDEMO-ADMIN-OPT-* tables; usage inferred) | 51 |
| [COADM01](../copybooks/COADM01.md) | WORKING_STORAGE | Defines BMS communication area for map COADM1A (COADM1AI/COADM1AO with fields like OPTIONI/O, OPTNnnnO, ERRMSGO/C), CARDDEMO-COMMAREA, and admin option table (CDEMO-ADMIN-OPT-COUNT/NUM/NAME/PGMNAME) | 53 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Title literals (CCDA-TITLE01/02 used for screen headers) | 55 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Date/time working storage structures (WS-CURDATE-DATA and components for formatting) | 56 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Error message constants (CCDA-MSG-INVALID-KEY) | 57 |
| [CSUSR01Y](../copybooks/CSUSR01Y.md) | WORKING_STORAGE | User security or profile structures (WS-USRSEC-FILE suggests usage; not directly visible) | 58 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | Standard CICS AID constants (DFHENTER, DFHPF3 for EIBAID evaluation) | 60 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | Standard CICS BMS attribute constants (DFHGREEN for error message color) | 61 |

## Data Flow

### Reads From
- **COADM1AI**: OPTIONI
  (Lines: 121)
- **CARDDEMO-COMMAREA**: CDEMO-PGM-REENTER
  (Lines: 91)
- **EIBAID**: DFHENTER, DFHPF3
  (Lines: 97)
- **CDEMO-ADMIN-OPT-***: CDEMO-ADMIN-OPT-COUNT, CDEMO-ADMIN-OPT-NUM(*), CDEMO-ADMIN-OPT-NAME(*), CDEMO-ADMIN-OPT-PGMNAME(*)
  (Lines: 132, 231, 141)

### Writes To
- **COADM1AO**: ERRMSGO, TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO, OPTN001O-OPTN010O, OPTIONO, ERRMSGC
  (Lines: 180)
- **CARDDEMO-COMMAREA**: CDEMO-FROM-TRANID, CDEMO-FROM-PROGRAM, CDEMO-PGM-CONTEXT, CDEMO-TO-PROGRAM, CDEMO-PGM-REENTER
  (Lines: 142, 113)

### Transformations
- **OPTIONI OF COADM1AI** → **WS-OPTION / OPTIONO OF COADM1AO**: VARYING trim trailing spaces, INSPECT REPLACING ALL ' ' BY '0', move to numeric WS-OPTION and echo to output
  (Lines: 121, 127)
- **CDEMO-ADMIN-OPT-NUM(WS-IDX) / CDEMO-ADMIN-OPT-NAME(WS-IDX)** → **OPTNnnnO OF COADM1AO (e.g. OPTN001O)**: VARYING WS-IDX 1 to CDEMO-ADMIN-OPT-COUNT: STRING num '. ' name INTO WS-ADMIN-OPT-TXT, EVALUATE WS-IDX to move to specific OPTN field
  (Lines: 231, 236)
- **FUNCTION CURRENT-DATE** → **CURDATEO / CURTIMEO OF COADM1AO**: MOVE to WS-CURDATE-DATA (from CSDAT01Y), reformat components to MM-DD-YY and HH:MM:SS
  (Lines: 207, 218)
- **CCDA-TITLE01 / CCDA-TITLE02 (from COTTL01Y)** → **TITLE01O / TITLE02O OF COADM1AO**: Direct MOVE of title literals to header fields
  (Lines: 209)
- **WS-TRANID / WS-PGMNAME** → **TRNNAMEO / PGMNAMEO OF COADM1AO**: Direct MOVE of static transaction/program names to header
  (Lines: 211)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Main control: handle condition, clear errors, branch on EIBCALEN=0 or reenter, receive map, evaluate AID (ENTER/PF3/OTHER)
- Calls: RETURN-TO-SIGNON-SCREEN, SEND-MENU-SCREEN, RECEIVE-MENU-SCREEN, PROCESS-ENTER-KEY
- Lines: 75-113

### PROCESS-ENTER-KEY
**Purpose:** Parse/validate option input, XCTL if valid/non-DUMMY or error/redisplay
- Called by: MAIN-PARA
- Calls: SEND-MENU-SCREEN
- Lines: 119-158

### RETURN-TO-SIGNON-SCREEN
**Purpose:** Default/set CDEMO-TO-PROGRAM to COSGN00C and XCTL
- Called by: MAIN-PARA
- Lines: 163-170

### SEND-MENU-SCREEN
**Purpose:** Build/populate map output and EXEC CICS SEND MAP ERASE
- Called by: MAIN-PARA, PROCESS-ENTER-KEY, PGMIDERR-ERR-PARA
- Calls: POPULATE-HEADER-INFO, BUILD-MENU-OPTIONS
- Lines: 175-187

### RECEIVE-MENU-SCREEN
**Purpose:** EXEC CICS RECEIVE MAP INTO COADM1AI with RESP codes
- Called by: MAIN-PARA
- Lines: 192-200

### POPULATE-HEADER-INFO
**Purpose:** Format current date/time, move titles/tran/pgm to header output fields
- Called by: SEND-MENU-SCREEN
- Lines: 205-224

### BUILD-MENU-OPTIONS
**Purpose:** VARYING loop to STRING and EVALUATE move admin opt table to OPTNnnnO fields
- Called by: SEND-MENU-SCREEN
- Lines: 229-266

### PGMIDERR-ERR-PARA
**Purpose:** Handle XCTL PGMIDERR: set 'not installed' message, send screen, RETURN
- Calls: SEND-MENU-SCREEN
- Lines: 270-283

## Error Handling

- **PGMIDERR (XCTL target program not found):** Invoke PGMIDERR-ERR-PARA: message 'This option is not installed', send menu, RETURN to loop
  (Lines: 78, 270)
- **Invalid AID key (not ENTER/PF3):** WS-ERR-FLG='Y', WS-MESSAGE=CCDA-MSG-INVALID-KEY, SEND-MENU-SCREEN
  (Lines: 103)
- **Invalid menu option (non-numeric/zero/out-of-range):** WS-ERR-FLG='Y', WS-MESSAGE='Please enter a valid option number...', SEND-MENU-SCREEN
  (Lines: 134)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| HANDLE CONDITION | PGMIDERR(PGMIDERR-ERR-PARA) | Catch XCTL program-id errors and handle gracefully | 78 |
| SEND | MAP('COADM1A') MAPSET('COADM01') FROM(COADM1AO) ERASE | Erase and display populated admin menu to user | 182 |
| RECEIVE | MAP('COADM1A') MAPSET('COADM01') INTO(COADM1AI) RESP(WS-RESP-CD) RESP2(WS-REAS-CD) | Receive user map input with response codes | 194 |
| XCTL PROGRAM | CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION) COMMAREA(CARDDEMO-COMMAREA) | No-return transfer to selected admin program | 146 |
| XCTL PROGRAM | CDEMO-TO-PROGRAM COMMAREA(CARDDEMO-COMMAREA) | No-return transfer to signon/exit program | 169 |
| RETURN | TRANSID(WS-TRANID='CA00') COMMAREA(CARDDEMO-COMMAREA) | End pseudoconversational cycle, schedule next CA00 invocation | 111 |

## Open Questions

- **Exact values/content of CDEMO-ADMIN-OPT-COUNT, -NUM(*), -NAME(*), -PGMNAME(*)**
  - Context: Defined in copybook COADM01; determines displayed options and XCTL targets (some marked 'DUMMY')
  - Suggestion: Expand/analyze copybook COADM01
- **Full field layouts of CARDDEMO-COMMAREA, COADM1AI, COADM1AO**
  - Context: Defined in COADM01; critical for data flow but details absent
  - Suggestion: Expand/analyze copybook COADM01
- **Purpose/usage of COCOM01Y, COADM02Y, CSUSR01Y**
  - Context: Copied but no direct field references visible in source
  - Suggestion: Expand copybooks and check for indirect usage
- **Programs/transactions that invoke CA00 or set initial CDEMO-TO-PROGRAM**
  - Context: Not evident from this source alone
  - Suggestion: Review CICS transaction definitions or caller programs like COSGN00C

---
*Generated by War Rig WAR_RIG*