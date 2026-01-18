# COTRN01C

**File:** COTRN01C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 2
**Analyzed:** 2026-01-18 16:56:48.066173

## Purpose

COTRN01C is a CICS online transaction (CT01) specifically designed to view details of a single credit card transaction record from the VSAM TRANSACT file using screen map COTRN1A. Users enter a transaction ID (or use pre-selected from commarea), the program reads the record via CICS READ UPDATE, populates screen fields with details like card number, amount, type, merchant info, and timestamps, or shows errors for invalid/not found IDs. Unlike transaction list programs, it focuses exclusively on inquiry/display of one record with navigation to menus/lists via PF keys.

**Business Context:** Part of CardDemo application for credit card transaction inquiry, allowing detailed view of individual transactions keyed by ID.
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 5, 23, 37, 106, 172, 177, 213, 230, 269

## Calling Context

**Entry Points:** CT01
**Linkage Section:** DFHCOMMAREA, LK-COMMAREA

## Inputs

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Receives commarea from calling program containing context like CDEMO-CT01-TRN-SELECTED transaction ID
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 78, 98, 103

### COTRN1AI
- **Type:** CICS_MAP
- **Description:** Screen input map capturing user-entered transaction ID in TRNIDINI
- **Copybook:** [COTRN01](../copybooks/COTRN01.md)
- **Lines:** 232

### TRANSACT
- **Type:** FILE_VSAM
- **Description:** VSAM file of transaction records, accessed by key TRAN-ID
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 39, 269

## Outputs

### COTRN1AO
- **Type:** CICS_MAP
- **Description:** Screen output map with header (titles, date/time), error messages, and transaction details for display
- **Copybook:** [COTRN01](../copybooks/COTRN01.md)
- **Lines:** 219

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated commarea returned to CICS with transaction context for potential reentry
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 136, 138

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [COSGN00C](./COSGN00C.md) | CICS_XCTL | Called on first entry (EIBCALEN=0) or when no prior program specified in RETURN-TO-PREV-SCREEN (defaults to sign-on) | 96 |
| [COMEN00C](./COMEN00C.md) | CICS_XCTL | Called on PF3 when CDEMO-FROM-PROGRAM is spaces/low-values (no prior program, defaults to main menu) | 117 |
| [COTRN00C](./COTRN00C.md) | CICS_XCTL | Called on PF5 to return to transaction list screen | 126 |

## Business Rules

### BR001: If transaction ID is empty or low-values on enter, display error and reprompt
**Logic:** EVALUATE checks TRNIDINI, sets error flag and message if invalid
**Conditions:** TRNIDINI OF COTRN1AI = SPACES OR LOW-VALUES
**Lines:** 147, 152

### BR002: If transaction not found in TRANSACT, set error flag and message, reprompt with cursor on ID field
**Logic:** After READ, check DFHRESP(NOTFND), move error message and -1 to TRNIDINL
**Conditions:** WS-RESP-CD = DFHRESP(NOTFND)
**Lines:** 280, 284

### BR003: On invalid AID key, set error and invalid key message
**Logic:** EVALUATE EIBAID WHEN OTHER
**Conditions:** EIBAID NOT IN (DFHENTER, DFHPF3, DFHPF4, DFHPF5)
**Lines:** 112, 129

### BR004: PF3 returns to prior program or default (COMEN01C if no prior, else CDEMO-FROM-PROGRAM), PF5 to COTRN00C transaction list
**Logic:** EVALUATE EIBAID sets CDEMO-TO-PROGRAM based on conditions
**Conditions:** EIBAID = DFHPF3, EIBAID = DFHPF5
**Lines:** 112, 116, 126

### BR005: PF4 clears all screen fields and resets for new transaction ID entry
**Logic:** Performs CLEAR-CURRENT-SCREEN which initializes fields to spaces/low-values and redisplays screen
**Conditions:** EIBAID = DFHPF4
**Lines:** 124

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Common CardDemo commarea including CDEMO-CT01-INFO for transaction selection and navigation context | 52 |
| [COTRN01](../copybooks/COTRN01.md) | WORKING_STORAGE | BMS map definitions for COTRN1AI (input) and COTRN1AO (output) transaction view screen | 63 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Transaction title/list related data structures | 65 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Date/time formatting structures like WS-CURDATE-DATA | 66 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Constants for messages (CCDA-MSG-INVALID-KEY) and titles (CCDA-TITLE01/02) | 67 |
| [CVTRA05Y](../copybooks/CVTRA05Y.md) | WORKING_STORAGE | Layout of TRAN-RECORD for VSAM TRANSACT file transactions | 69 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | CICS attention identifier constants (DFHENTER, DFHPF3 etc.) | 71 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | BMS symbolic constants for mapset/map handling | 72 |

## Data Flow

### Reads From
- **TRANSACT**: TRAN-ID, TRAN-AMT, TRAN-CARD-NUM, TRAN-TYPE-CD, TRAN-CAT-CD, TRAN-SOURCE, TRAN-DESC, TRAN-ORIG-TS, TRAN-PROC-TS, TRAN-MERCHANT-ID, TRAN-MERCHANT-NAME, TRAN-MERCHANT-CITY, TRAN-MERCHANT-ZIP
  (Lines: 172, 269, 277)
- **COTRN1AI**: TRNIDINI
  (Lines: 106, 147, 172)
- **CARDDEMO-COMMAREA**: CDEMO-CT01-TRN-SELECTED
  (Lines: 98, 103)

### Writes To
- **COTRN1AO**: ERRMSGO, TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO
  (Lines: 217, 247)
- **COTRN1AI**: TRNIDI, CARDNUMI, TTYPCDI, TCATCDI, TRNSRCI, TRNAMTI, TDESCI, TORIGDTI, TPROCDTI, MIDI, MNAMEI, MCITYI, MZIPI
  (Lines: 178)
- **CDEMO-TO-PROGRAM**: COSGN00C, COMEN01C, COTRN00C
  (Lines: 96, 117, 126)

### Transformations
- **TRAN-AMT** → **WS-TRAN-AMT**: Direct move from TRAN-RECORD to WS for numeric formatting before screen move
  (Lines: 177)
- **TRAN-ID** → **TRNIDI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 178)
- **TRAN-CARD-NUM** → **CARDNUMI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 179)
- **TRAN-TYPE-CD** → **TTYPCDI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 180)
- **TRAN-CAT-CD** → **TCATCDI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 181)
- **TRAN-SOURCE** → **TRNSRCI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 182)
- **WS-TRAN-AMT** → **TRNAMTI OF COTRN1AI**: Move from WS (formatted from TRAN-AMT) to screen amount field
  (Lines: 183)
- **TRAN-DESC** → **TDESCI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 184)
- **TRAN-ORIG-TS** → **TORIGDTI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 185)
- **TRAN-PROC-TS** → **TPROCDTI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 186)
- **TRAN-MERCHANT-ID** → **MIDI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 187)
- **TRAN-MERCHANT-NAME** → **MNAMEI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 188)
- **TRAN-MERCHANT-CITY** → **MCITYI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 189)
- **TRAN-MERCHANT-ZIP** → **MZIPI OF COTRN1AI**: Direct move from file record to screen display field
  (Lines: 190)
- **FUNCTION CURRENT-DATE** → **CURDATEO OF COTRN1AO**: Parse current date into MM-DD-YY format for screen header
  (Lines: 245, 256)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Main control: initializes, handles first/reentry logic, receives map, evaluates AID keys for actions
- Calls: RETURN-TO-PREV-SCREEN, PROCESS-ENTER-KEY, SEND-TRNVIEW-SCREEN, RECEIVE-TRNVIEW-SCREEN, CLEAR-CURRENT-SCREEN
- Lines: 86-138

### PROCESS-ENTER-KEY
**Purpose:** Validates/processes transaction ID input, clears other fields, reads file, populates display or errors
- Called by: MAIN-PARA
- Calls: SEND-TRNVIEW-SCREEN, READ-TRANSACT-FILE
- Lines: 144-192

### RETURN-TO-PREV-SCREEN
**Purpose:** Prepares and XCTLs to target program (sets from context or defaults)
- Called by: MAIN-PARA
- Lines: 197-207

### SEND-TRNVIEW-SCREEN
**Purpose:** Populates header/messages, sends map with ERASE CURSOR
- Called by: MAIN-PARA, PROCESS-ENTER-KEY, CLEAR-CURRENT-SCREEN, READ-TRANSACT-FILE
- Calls: POPULATE-HEADER-INFO
- Lines: 213-225

### RECEIVE-TRNVIEW-SCREEN
**Purpose:** Receives user map data into COTRN1AI capturing resp codes
- Called by: MAIN-PARA
- Lines: 230-237

### POPULATE-HEADER-INFO
**Purpose:** Formats current date/time, moves static titles/program names to output map
- Called by: SEND-TRNVIEW-SCREEN
- Lines: 243-262

### READ-TRANSACT-FILE
**Purpose:** CICS READ UPDATE TRANSACT by TRAN-ID, evaluates RESP for normal/error handling
- Called by: PROCESS-ENTER-KEY
- Calls: SEND-TRNVIEW-SCREEN
- Lines: 267-296

### CLEAR-CURRENT-SCREEN
**Purpose:** Clears/resets screen by initializing fields and resending
- Called by: MAIN-PARA
- Calls: INITIALIZE-ALL-FIELDS, SEND-TRNVIEW-SCREEN
- Lines: 301-304

### INITIALIZE-ALL-FIELDS
**Purpose:** Sets screen fields to spaces/low-values and clears message
- Called by: CLEAR-CURRENT-SCREEN
- Lines: 309-326

## Error Handling

- **TRNIDINI empty or low-values:** Set WS-ERR-FLG 'Y', custom error message, TRNIDINL -1, send screen
  (Lines: 147)
- **READ RESP = DFHRESP(NOTFND):** Set WS-ERR-FLG 'Y', 'Transaction ID NOT found' message, TRNIDINL -1, send screen
  (Lines: 284)
- **READ RESP OTHER than NORMAL/NOTFND:** DISPLAY RESP/REAS, set error flag, 'Unable to lookup' message, TRNIDINL -1, send screen
  (Lines: 290)
- **Invalid EIBAID:** Set WS-ERR-FLG 'Y', CCDA-MSG-INVALID-KEY message, send screen
  (Lines: 129)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| RETURN | TRANSID(WS-TRANID) | Returns to CICS with CT01 transid and updated commarea | 136 |
| SEND | MAP('COTRN1A') MAPSET('COTRN01') | Displays transaction view screen from COTRN1AO with erase and cursor | 219 |
| RECEIVE | MAP('COTRN1A') MAPSET('COTRN01') | Captures user input into COTRN1AI with RESP codes | 232 |
| READ | DATASET(TRANSACT) | Reads VSAM transaction record by RIDFLD(TRAN-ID) into TRAN-RECORD for UPDATE | 269 |
| XCTL | PROGRAM(CDEMO-TO-PROGRAM) | Transfers control to prior/target program (COSGN00C/COMEN01C/COTRN00C) with commarea | 205 |

## Open Questions

- **Exact field layouts in copybooks (e.g., CVTRA05Y TRAN-RECORD fields, COTRN01 map fields)**
  - Context: COPY referenced but source not provided; field names inferred from moves
  - Suggestion: Obtain and analyze copybook sources
- **Programs/transids calling COTRN01C (CT01)**
  - Context: Inferred from context (e.g., COTRN00C list), but no explicit callers
  - Suggestion: Review CICS region definitions or caller code
- **Implications of READ UPDATE without REWRITE**
  - Context: File opened UPDATE but only read; may lock record or abend?
  - Suggestion: Check CICS VSAM file control options or runtime behavior

---
*Generated by War Rig WAR_RIG*