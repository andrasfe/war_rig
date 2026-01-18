# COTRN00C

**File:** COTRN00C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 1
**Analyzed:** 2026-01-18 16:09:32.512324

## Purpose

CICS online program that displays a paginated list of transactions from the TRANSACT VSAM file on map COTRN0A. Handles user selections to view transaction details via XCTL to COTRN01C, paging forward/backward with PF8/PF7, and returns to previous screens. Processes input from map COTRN0AI and populates output map COTRN0AO with transaction data.

**Business Context:** CardDemo application for listing and selecting card transactions.
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 5, 95, 527, 554

## Calling Context

**Entry Points:** CT00
**Linkage Section:** DFHCOMMAREA, LK-COMMAREA, CARDDEMO-COMMAREA

## Inputs

### COTRN0AI
- **Type:** CICS_MAP
- **Description:** Input map fields for transaction list screen including selection fields SEL0001I-SEL0010I, transaction IDs TRNID01I-TRNID10I, input transaction ID TRNIDINI, and page number PAGENUMI
- **Copybook:** [COTRN00](../copybooks/COTRN00.md)
- **Lines:** 556, 148, 206

### TRANSACT
- **Type:** FILE_VSAM
- **Description:** VSAM dataset containing transaction records with key TRAN-ID
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 39, 626, 660

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Incoming commarea copied to CARDDEMO-COMMAREA for program context and navigation
- **Lines:** 87, 111

## Outputs

### COTRN0AO
- **Type:** CICS_MAP
- **Description:** Output map fields for transaction list screen including header info TITLE01O-TITLE02O, error message ERRMSGO, transaction display fields TRNID01O-TDATE10O etc., current date/time CURDATEO CURTIMEO
- **Copybook:** [COTRN00](../copybooks/COTRN00.md)
- **Lines:** 534, 542

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Outgoing commarea passed to XCTL programs and RETURN TRANSID
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 138, 194, 520

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [COTRN01C](./COTRN01C.md) | CICS_XCTL | Display details of selected transaction | 193 |
| [COSGN00C](./COSGN00C.md) | CICS_XCTL | Return to sign-on screen | 109 |
| [COMEN01C](./COMEN01C.md) | CICS_XCTL | Return to main menu screen on PF3 | 123 |

## Business Rules

### BR001: If user enters 'S' or 's' in any SEL000xI field with corresponding TRNIDxxI, XCTL to COTRN01C with selected transaction ID in commarea
**Logic:** EVALUATE on SEL fields to set CDEMO-CT00-TRN-SEL-FLG and CDEMO-CT00-TRN-SELECTED, then check for 'S'/'s' and XCTL
**Conditions:** SEL0001I OF COTRN0AI NOT = SPACES AND LOW-VALUES, CDEMO-CT00-TRN-SEL-FLG = 'S' OR 's'
**Lines:** 148, 185

### BR002: Input transaction ID TRNIDINI must be numeric or spaces/LOW-VALUES; else error message and redisplay screen
**Logic:** Check NUMERIC on TRNIDINI, set TRAN-ID, error if not
**Conditions:** TRNIDINI OF COTRN0AI = SPACES OR LOW-VALUES, TRNIDINI OF COTRN0AI IS NUMERIC
**Lines:** 206, 209

### BR003: PF7 (backward) starts browse from first TRNID on screen or LOW-VALUES, reads 10 previous records
**Logic:** Set TRAN-ID to CDEMO-CT00-TRNID-FIRST or LOW-VALUES, STARTBR, loop READPREV 10 times populating screen fields
**Conditions:** EIBAID = DFHPF7, CDEMO-CT00-PAGE-NUM > 1
**Lines:** 234, 236, 333

### BR004: Display up to 10 transactions per page, track first/last TRNID and NEXT-PAGE-YES/NO for paging
**Logic:** Loop WS-IDX 1-10, READNEXT/PREV, POPULATE-TRAN-DATA, set first/last and peek next for more pages
**Conditions:** TRANSACT-NOT-EOF, ERR-FLG-OFF
**Lines:** 290, 297, 350

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Defines CARDDEMO-COMMAREA including CDEMO-CT00-INFO for paging and selection context | 61 |
| [COTRN00](../copybooks/COTRN00.md) | WORKING_STORAGE | Defines input/output map areas COTRN0AI and COTRN0AO for transaction list screen | 72 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Defines titles CCDA-TITLE01 CCDA-TITLE02 used in header | 74 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Defines date formatting fields WS-CURDATE-* WS-CURTIME-* | 75 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Defines error messages like CCDA-MSG-INVALID-KEY | 76 |
| [CVTRA05Y](../copybooks/CVTRA05Y.md) | WORKING_STORAGE | Defines TRAN-RECORD structure with TRAN-ID key, TRAN-AMT, TRAN-ORIG-TS, TRAN-DESC | 78 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | Defines EIBAID attention key values like DFHENTER DFHPF7 | 80 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | Defines EIBCALEN and other CICS fields | 81 |

## Data Flow

### Reads From
- **TRANSACT**: TRAN-ID, TRAN-AMT, TRAN-ORIG-TS, TRAN-DESC
  (Lines: 626, 382)
- **COTRN0AI**: SEL0001I, TRNID01I, TRNIDINI, PAGENUMI
  (Lines: 148, 206)

### Writes To
- **COTRN0AO**: TRNID01O, TDATE01O, TDESC01O, TAMT001O, ERRMSGO, CURDATEO, CURTIMEO
  (Lines: 532, 324, 580)
- **CDEMO-CT00-INFO**: CDEMO-CT00-TRNID-FIRST, CDEMO-CT00-TRNID-LAST, CDEMO-CT00-PAGE-NUM, CDEMO-CT00-NEXT-PAGE-FLG
  (Lines: 393, 438, 306)

### Transformations
- **TRAN-ORIG-TS** → **WS-TRAN-DATE**: Extract YYYYMMDD from timestamp, format to MM/DD/YY
  (Lines: 385)
- **TRAN-AMT** → **WS-TRAN-AMT**: Direct move to display format PIC +99999999.99
  (Lines: 383)
- **WS-CURDATE-DATA** → **CURDATEO**: Format CURRENT-DATE to MM/DD/YY
  (Lines: 569)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Main control logic: initialize flags, handle first entry vs reentry, process AID keys, RETURN with TRANSID
- Calls: RETURN-TO-PREV-SCREEN, PROCESS-ENTER-KEY, SEND-TRNLST-SCREEN, RECEIVE-TRNLST-SCREEN, PROCESS-PF7-KEY, PROCESS-PF8-KEY
- Lines: 95-141

### PROCESS-ENTER-KEY
**Purpose:** Process selection fields and input TRNIDINI, handle 'S' selection XCTL, load first page
- Called by: MAIN-PARA
- Calls: SEND-TRNLST-SCREEN, PROCESS-PAGE-FORWARD
- Lines: 146-229

### PROCESS-PF7-KEY
**Purpose:** Handle backward paging from first TRNID
- Called by: MAIN-PARA
- Calls: PROCESS-PAGE-BACKWARD, SEND-TRNLST-SCREEN
- Lines: 234-252

### PROCESS-PF8-KEY
**Purpose:** Handle forward paging from last TRNID
- Called by: MAIN-PARA
- Calls: PROCESS-PAGE-FORWARD, SEND-TRNLST-SCREEN
- Lines: 257-274

### PROCESS-PAGE-FORWARD
**Purpose:** Browse forward: STARTBR, read up to 10 records, populate screen, check for more pages
- Called by: PROCESS-ENTER-KEY, PROCESS-PF8-KEY
- Calls: STARTBR-TRANSACT-FILE, READNEXT-TRANSACT-FILE, INITIALIZE-TRAN-DATA, POPULATE-TRAN-DATA, ENDBR-TRANSACT-FILE, SEND-TRNLST-SCREEN
- Lines: 279-328

### POPULATE-TRAN-DATA
**Purpose:** Move current TRAN-RECORD fields to screen fields TRNIDxxI TDATExxI etc. based on WS-IDX, set first/last
- Called by: PROCESS-PAGE-FORWARD, PROCESS-PAGE-BACKWARD
- Lines: 381-445

### SEND-TRNLST-SCREEN
**Purpose:** Populate header, send map COTRN0A with/without ERASE
- Called by: MAIN-PARA, PROCESS-ENTER-KEY, PROCESS-PF7-KEY, PROCESS-PF8-KEY, PROCESS-PAGE-FORWARD, PROCESS-PAGE-BACKWARD
- Calls: POPULATE-HEADER-INFO
- Lines: 527-549

## Error Handling

- **WS-RESP-CD NOT = DFHRESP(NORMAL) OR NOTFND:** Set WS-ERR-FLG 'Y', move error message to WS-MESSAGE, set TRNIDINL -1, PERFORM SEND-TRNLST-SCREEN
  (Lines: 602)
- **TRNIDINI not numeric:** Set WS-ERR-FLG 'Y', error message, TRNIDINL -1, SEND-TRNLST-SCREEN
  (Lines: 212)
- **Invalid EIBAID:** Set WS-ERR-FLG 'Y', invalid key message, SEND-TRNLST-SCREEN
  (Lines: 130)
- **READNEXT/READPREV ENDFILE:** Set TRANSACT-EOF, message about end/top of file, SEND-TRNLST-SCREEN
  (Lines: 636, 670)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| RECEIVE MAP | COTRN0A | Receive user input into COTRN0AI | 556 |
| SEND MAP | COTRN0A | Send transaction list screen from COTRN0AO with CURSOR and optional ERASE | 534 |
| STARTBR | TRANSACT | Start browse on TRAN-ID key for forward/backward paging | 593 |
| READNEXT | TRANSACT | Read next transaction record into TRAN-RECORD | 626 |
| READPREV | TRANSACT | Read previous transaction record into TRAN-RECORD | 660 |
| ENDBR | TRANSACT | End browse after paging | 694 |
| XCTL | COTRN01C | Transfer control to transaction detail program | 193 |
| RETURN |  | Return to CICS with next TRANSID CT00 and commarea | 138 |

## Open Questions

- **Exact structure and lengths of fields in copybooks like CVTRA05Y TRAN-RECORD, COTRN00 map fields**
  - Context: Copybooks not provided in source; inferred from usage but details UNKNOWN
  - Suggestion: Analyze copybook source files
- **Called_by programs and full navigation flow**
  - Context: No static analysis of callers; only outgoing XCTL/RETURN visible
  - Suggestion: Trace transaction flow from sign-on COSGN00C
- **VSAM file organization of TRANSACT**
  - Context: Assumed KSDS from STARTBR RIDFLD KEYLENGTH; no FD/SELECT
  - Suggestion: Check CICS FCT definitions

---
*Generated by War Rig WAR_RIG*