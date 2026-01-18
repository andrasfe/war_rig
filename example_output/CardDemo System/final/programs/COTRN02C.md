# COTRN02C

**File:** COTRN02C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 1
**Analyzed:** 2026-01-18 16:59:19.911158

## Purpose

CICS online program that presents a screen (COTRN2A) for entering new transaction details including account/card, type, amount, dates, merchant info; validates inputs against rules and cross-reference files (CXACAIX, CCXREF); generates next TRAN-ID by reading previous record in TRANSACT file; writes new transaction if confirmed.

**Business Context:** CardDemo application for adding credit card transactions to TRANSACT VSAM file
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 5, 107, 516, 539, 711

## Calling Context

**Called By:** [COSGN00C](./COSGN00C.md), [COMEN01C](./COMEN01C.md)
**Entry Points:** CT02
**Linkage Section:** DFHCOMMAREA

## Inputs

### COTRN2AI
- **Type:** CICS_MAP
- **Description:** Input map fields for transaction add: ACTIDINI (account ID), CARDNINI (card num), TTYPCDI (type cd), TCATCDI (cat cd), TRNSRCI (source), TRNAMTI (amt), TDESCI (desc), TORIGDTI/PROCDTI (dates), MIDI/MNAMEI/MCITYI/MZIPI (merchant), CONFIRMI
- **Copybook:** [COTRN02](../copybooks/COTRN02.md)
- **Lines:** 539, 193, 235

### CARD-XREF-RECORD
- **Type:** FILE_VSAM
- **Description:** Cross-reference record from CXACAIX (acct to card) or CCXREF (card to acct)
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 578, 611

### TRAN-RECORD
- **Type:** FILE_VSAM
- **Description:** Previous transaction record from TRANSACT to get last TRAN-ID
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 675

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** CARDDEMO-COMMAREA passed between programs containing CDEMO-CT02-* fields and context
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 99, 119

### CSUTLDTC-PARM
- **Type:** PARAMETER
- **Description:** Parameters for CSUTLDTC date validation call: date string and format
- **Lines:** 62, 393

## Outputs

### COTRN2AO
- **Type:** CICS_MAP
- **Description:** Output map with header (titles, dates, times, pgm/tran names), error messages, and fields
- **Copybook:** [COTRN02](../copybooks/COTRN02.md)
- **Lines:** 522

### TRAN-RECORD
- **Type:** FILE_VSAM
- **Description:** New transaction record written to TRANSACT with generated TRAN-ID and input fields
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 713

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated commarea returned or XCTL'd to next program
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 532, 509

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CSUTLDTC](./CSUTLDTC.md) | STATIC_CALL | Validate date format conversion YYYY-MM-DD | 393 |
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | Transfer control to previous screen/program (e.g., COSGN00C or COMEN01C) | 509 |

## Business Rules

### BR001: Account ID or Card Number must be entered and numeric; cross-ref other via CXACAIX/CCXREF
**Logic:** EVALUATE TRUE on ACTIDINI/CARDNINI; NUMVAL to numeric; READ file; error if NOTFND
**Conditions:** ACTIDINI/CARDNINI NOT SPACES/LOW-VALUES, IS NOT NUMERIC, DFHRESP(NOTFND)
**Lines:** 193, 208, 222

### BR002: All data fields (type cd, cat cd, source, desc, amt, orig/proc dates, merchant id/name/city/zip) must be non-empty
**Logic:** EVALUATE TRUE checks each = SPACES/LOW-VALUES; set error and cursor
**Conditions:** = SPACES OR LOW-VALUES
**Lines:** 251, 252, 258, 264, 270, 276, 282, 288, 294, 300, 306, 312

### BR003: Transaction amount in format [+/-]99999999.99
**Logic:** Check sign, digits, decimal; NUMVAL-C to validate/edit
**Conditions:** (1:1) NOT '-' OR '+', (2:8) NOT NUMERIC, (10:1) != '.', (11:2) NOT NUMERIC
**Lines:** 339, 340

### BR004: Orig/Proc dates in YYYY-MM-DD validated by CSUTLDTC (not msg 2513)
**Logic:** Format checks + CALL CSUTLDTC; error if SEV-CD !=0000 and MSG-NUM !=2513
**Conditions:** NUMERIC checks on parts, CSUTLDTC-RESULT-SEV-CD != '0000', CSUTLDTC-RESULT-MSG-NUM != '2513'
**Lines:** 353, 368, 393, 413

### BR005: Merchant ID numeric
**Logic:** IS NOT NUMERIC check
**Conditions:** MIDI IS NOT NUMERIC
**Lines:** 430

### BR006: Confirm Y/N to add transaction
**Logic:** EVALUATE CONFIRMI; Y add, else error message
**Conditions:** CONFIRMI = 'Y'/'y', OTHER
**Lines:** 169

### BR007: New TRAN-ID = previous max +1
**Logic:** STARTBR HIGH-VALUES, READPREV, ADD 1; ZEROS if ENDFILE
**Conditions:** DFHRESP(ENDFILE)
**Lines:** 444, 448

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Common commarea: CDEMO-CT02-INFO (tranid first/last, page, selected), CDEMO-PGM-REENTER, CDEMO-FROM/TO-PROGRAM | 71 |
| [COTRN02](../copybooks/COTRN02.md) | WORKING_STORAGE | BMS map structures COTRN2AI (input) / COTRN2AO (output) for transaction add screen | 82 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Titles CCDA-TITLE01/02 | 84 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Date/time working fields WS-CURDATE-DATA etc. | 85 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Messages CCDA-MSG-INVALID-KEY | 86 |
| [CVTRA05Y](../copybooks/CVTRA05Y.md) | WORKING_STORAGE | TRAN-RECORD transaction structure: TRAN-ID, TYPE-CD, CAT-CD, SOURCE, DESC, AMT, CARD-NUM, MERCHANT-*, ORIG/PROC-TS | 88 |
| [CVACT01Y](../copybooks/CVACT01Y.md) | WORKING_STORAGE | CARD-XREF-RECORD: XREF-ACCT-ID, XREF-CARD-NUM | 89 |
| [CVACT03Y](../copybooks/CVACT03Y.md) | WORKING_STORAGE | UNKNOWN (likely related to account/card) | 90 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | CICS aid keys DFHENTER, DFHPF3 etc. | 92 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | CICS BMS constants | 93 |

## Data Flow

### Reads From
- **COTRN2AI**: ACTIDINI, CARDNINI, TTYPCDI, TCATCDI, TRNSRCI, TDESCI, TRNAMTI, TORIGDTI, TPROCDTI, MIDI, MNAMEI, MCITYI, MZIPI, CONFIRMI
  (Lines: 193, 235, 452)
- **CARD-XREF-RECORD**: XREF-ACCT-ID, XREF-CARD-NUM
  (Lines: 209, 223)
- **TRAN-RECORD**: TRAN-ID, TRAN-AMT, TRAN-TYPE-CD, TRAN-CAT-CD, TRAN-SOURCE, TRAN-DESC, TRAN-ORIG-TS, TRAN-PROC-TS, TRAN-MERCHANT-ID, TRAN-MERCHANT-NAME, TRAN-MERCHANT-CITY, TRAN-MERCHANT-ZIP
  (Lines: 446, 481)

### Writes To
- **TRAN-RECORD**: TRAN-ID, TRAN-TYPE-CD, TRAN-CAT-CD, TRAN-SOURCE, TRAN-DESC, TRAN-AMT, TRAN-CARD-NUM, TRAN-MERCHANT-ID, TRAN-MERCHANT-NAME, TRAN-MERCHANT-CITY, TRAN-MERCHANT-ZIP, TRAN-ORIG-TS, TRAN-PROC-TS
  (Lines: 451, 452, 466)
- **COTRN2AO**: ERRMSGO, TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO, ERRMSGC
  (Lines: 520, 556)

### Transformations
- **ACTIDINI/CARDNINI** → **XREF-ACCT-ID/XREF-CARD-NUM**: NUMVAL to numeric WS-ACCT-ID-N/WS-CARD-NUM-N then MOVE
  (Lines: 204, 218)
- **TRNAMTI** → **TRAN-AMT/WS-TRAN-AMT-E**: NUMVAL-C to edited numeric/comp
  (Lines: 383, 456)
- **TRAN-ID (prev)** → **TRAN-ID (new)**: ADD 1 to numeric WS-TRAN-ID-N
  (Lines: 448)
- **TORIGDTI/TPROCDTI** → **CSUTLDTC-DATE**: Direct MOVE for validation
  (Lines: 389, 409)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Main control: init flags/msg, handle first entry vs reenter, EIBAID PF/ENTER processing
- Calls: RETURN-TO-PREV-SCREEN, PROCESS-ENTER-KEY, SEND-TRNADD-SCREEN, RECEIVE-TRNADD-SCREEN, CLEAR-CURRENT-SCREEN, COPY-LAST-TRAN-DATA
- Lines: 107-159

### PROCESS-ENTER-KEY
**Purpose:** Invoke key/data validation then EVALUATE CONFIRMI to add or error
- Called by: MAIN-PARA
- Calls: VALIDATE-INPUT-KEY-FIELDS, VALIDATE-INPUT-DATA-FIELDS, ADD-TRANSACTION, SEND-TRNADD-SCREEN
- Lines: 164-187

### VALIDATE-INPUT-KEY-FIELDS
**Purpose:** Validate/lookup acct/card cross-ref
- Called by: PROCESS-ENTER-KEY
- Calls: SEND-TRNADD-SCREEN, READ-CXACAIX-FILE, READ-CCXREF-FILE
- Lines: 193-229

### VALIDATE-INPUT-DATA-FIELDS
**Purpose:** Validate all data fields empty/numeric/format/date via CSUTLDTC
- Called by: PROCESS-ENTER-KEY
- Calls: SEND-TRNADD-SCREEN
- Lines: 235-437

### ADD-TRANSACTION
**Purpose:** Generate next TRAN-ID, populate/populate TRAN-RECORD from input, WRITE
- Called by: PROCESS-ENTER-KEY
- Calls: STARTBR-TRANSACT-FILE, READPREV-TRANSACT-FILE, ENDBR-TRANSACT-FILE, WRITE-TRANSACT-FILE
- Lines: 442-466

### COPY-LAST-TRAN-DATA
**Purpose:** PF5: copy previous tran fields to screen (after key validate)
- Called by: MAIN-PARA
- Calls: VALIDATE-INPUT-KEY-FIELDS, STARTBR-TRANSACT-FILE, READPREV-TRANSACT-FILE, ENDBR-TRANSACT-FILE, PROCESS-ENTER-KEY
- Lines: 471-495

### RETURN-TO-PREV-SCREEN
**Purpose:** PF3: XCTL to CDEMO-TO-PROGRAM (default COSGN00C)
- Called by: MAIN-PARA
- Lines: 500-510

### SEND-TRNADD-SCREEN
**Purpose:** Populate header, move msg, SEND MAP ERASE CURSOR, RETURN transid CT02
- Called by: MAIN-PARA, PROCESS-ENTER-KEY, VALIDATE-INPUT-*, ADD-TRANSACTION, WRITE-*, CLEAR-*
- Calls: POPULATE-HEADER-INFO
- Lines: 516-534

### RECEIVE-TRNADD-SCREEN
**Purpose:** RECEIVE MAP INTO COTRN2AI
- Called by: MAIN-PARA
- Lines: 539-547

### POPULATE-HEADER-INFO
**Purpose:** Set titles, tran/pgmn names, current date/time formatted
- Called by: SEND-TRNADD-SCREEN
- Lines: 552-571

### READ-CXACAIX-FILE
**Purpose:** READ VSAM CXACAIX by acct for card num
- Called by: VALIDATE-INPUT-KEY-FIELDS
- Calls: SEND-TRNADD-SCREEN
- Lines: 576-604

### READ-CCXREF-FILE
**Purpose:** READ VSAM CCXREF by card for acct
- Called by: VALIDATE-INPUT-KEY-FIELDS
- Calls: SEND-TRNADD-SCREEN
- Lines: 609-637

### STARTBR-TRANSACT-FILE
**Purpose:** STARTBR TRANSACT HIGH-VALUES on TRAN-ID
- Called by: ADD-TRANSACTION, COPY-LAST-TRAN-DATA
- Calls: SEND-TRNADD-SCREEN
- Lines: 642-672

### READPREV-TRANSACT-FILE
**Purpose:** READPREV TRANSACT for last record
- Called by: ADD-TRANSACTION, COPY-LAST-TRAN-DATA
- Calls: SEND-TRNADD-SCREEN
- Lines: 673-697

### ENDBR-TRANSACT-FILE
**Purpose:** ENDBR TRANSACT
- Called by: ADD-TRANSACTION, COPY-LAST-TRAN-DATA
- Lines: 702-706

### WRITE-TRANSACT-FILE
**Purpose:** WRITE new TRAN-RECORD; handle NORMAL/DUP/OTHER
- Called by: ADD-TRANSACTION
- Calls: INITIALIZE-ALL-FIELDS, SEND-TRNADD-SCREEN
- Lines: 711-749

### CLEAR-CURRENT-SCREEN
**Purpose:** PF4: init fields and send screen
- Called by: MAIN-PARA
- Calls: INITIALIZE-ALL-FIELDS, SEND-TRNADD-SCREEN
- Lines: 754-757

### INITIALIZE-ALL-FIELDS
**Purpose:** MOVE SPACES/-1 to all input fields and msg
- Called by: WRITE-TRANSACT-FILE, CLEAR-CURRENT-SCREEN
- Lines: 762-779

## Error Handling

- **Input fields empty/non-numeric/invalid format:** SET WS-ERR-FLG 'Y', MOVE error msg to WS-MESSAGE, cursor -1 to field, PERFORM SEND-TRNADD-SCREEN
  (Lines: 199, 226, 254, 278, 384)
- **File READ/STARTBR/READPREV RESP NOT NORMAL/NOTFND/OTHER:** SET WS-ERR-FLG 'Y', specific msg, cursor to input field, SEND screen; DISPLAY RESP/REAS
  (Lines: 592, 625, 655, 696)
- **CSUTLDTC SEV-CD !=0000 and MSG-NUM !=2513:** Error msg, cursor to date field, SEND screen
  (Lines: 400, 420)
- **WRITE RESP DUPKEY/DUPREC/OTHER:** Error msg, cursor ACTIDINL, SEND screen
  (Lines: 736, 743)
- **Invalid EIBAID:** SET ERR-FLG, invalid key msg, SEND screen
  (Lines: 150)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| RECEIVE MAP | COTRN2A (MAPSET COTRN02) | Receive screen input into COTRN2AI | 541 |
| SEND MAP | COTRN2A (MAPSET COTRN02) | Send screen output from COTRN2AO with ERASE CURSOR | 522 |
| READ DATASET | CXACAIX/CCXREF | Read cross-ref records by key | 578 |
| STARTBR | TRANSACT | Start browse backward from HIGH-VALUES on TRAN-ID | 644 |
| READPREV | TRANSACT | Read previous (highest) transaction | 675 |
| ENDBR | TRANSACT | End browse | 704 |
| WRITE DATASET | TRANSACT | Write new transaction record | 713 |
| RETURN TRANSID | CT02 COMMAREA | Return to CICS with commarea | 156 |
| XCTL PROGRAM | CDEMO-TO-PROGRAM COMMAREA | Transfer to previous program | 509 |

## Open Questions

- **Exact field layouts in copybooks like CVTRA05Y, CVACT01Y, COTRN02**
  - Context: Source shows usage but not full 01-level definitions
  - Suggestion: Analyze copybook source files
- **Purpose of CVACT03Y and unused WS-ACCTDAT-FILE**
  - Context: Copied but no usage in code
  - Suggestion: Check if legacy or conditional
- **Full business validation (e.g., valid type/cat codes)**
  - Context: Only format/required/notfound checked; no code tables
  - Suggestion: Review external validation tables

---
*Generated by War Rig WAR_RIG*