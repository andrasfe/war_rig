# COBIL00C

**File:** COBIL00C.cbl
**Type:** COBOL
**Status:** FinalStatus.VALHALLA
**Iterations:** 2
**Analyzed:** 2026-01-18 16:44:35.645947

## Purpose

COBIL00C is a CICS online program that displays a bill payment screen (COBIL0A), receives user input for account ID and Y/N confirmation, reads account details from ACCTDAT and CXACAIX files, generates sequential transaction ID, writes bill payment record to TRANSACT if confirmed (paying full balance), updates account balance to zero, and handles aid keys ENTER/PF3/PF4.

**Business Context:** Provides online bill payment functionality in CardDemo application, simulating POS terminal full balance payments with transaction logging.
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 5, 24, 38, 99

## Calling Context

**Entry Points:** CB00
**Linkage Section:** DFHCOMMAREA

## Inputs

### COBIL0AI
- **Type:** CICS_MAP
- **Description:** Bill pay screen input: ACTIDINI (account ID), CONFIRMI (Y/N confirm), CURBALI (balance display)
- **Copybook:** [COBIL00](../copybooks/COBIL00.md)
- **Lines:** 308, 312

### ACCTDAT
- **Type:** FILE_VSAM
- **Description:** Account record with ACCT-ID key, ACCT-CURR-BAL balance, XREF-ACCT-ID
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 345, 347

### CXACAIX
- **Type:** FILE_VSAM
- **Description:** Card cross-reference record keyed by XREF-ACCT-ID, provides XREF-CARD-NUM
- **Copybook:** [CSDAT01Y](../copybooks/CSDAT01Y.md)
- **Lines:** 410, 412

### TRANSACT
- **Type:** FILE_VSAM
- **Description:** Transaction records for highest TRAN-ID generation via backward browse
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 443, 474

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Shared context: CDEMO-CB00-TRN-SELECTED (pre-selected acct ID), CDEMO-PGM-REENTER flag
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 111, 116

### EIBAID
- **Type:** CICS_COMMAREA
- **Description:** Aid key: DFHENTER (process), DFHPF3 (exit), DFHPF4 (clear)
- **Copybook:** [DFHAID](../copybooks/DFHAID.md)
- **Lines:** 125

## Outputs

### COBIL0AO
- **Type:** CICS_MAP
- **Description:** Bill pay screen output: ERRMSGO (messages), header titles/dates/times, cursor position
- **Copybook:** [COBIL00](../copybooks/COBIL00.md)
- **Lines:** 295, 299

### TRANSACT
- **Type:** FILE_VSAM
- **Description:** New bill payment transaction: sequential TRAN-ID, type '02', full ACCT-CURR-BAL as TRAN-AMT, timestamps
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 512

### ACCTDAT
- **Type:** FILE_VSAM
- **Description:** Updated account: ACCT-CURR-BAL set to zero post-payment
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 379

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated for return/XCTL: CDEMO-TO-PROGRAM, CDEMO-FROM-TRANID/PGMNAME
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 146, 282

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | Exit to prior program (COSGN00C/COMEN01C) on PF3/initial entry | 282 |

## Business Rules

### BR001: Account ID must not be empty/low-values
**Logic:** Error if ACTIDINI = SPACES/LOW-VALUES
**Conditions:** ACTIDINI OF COBIL0AI = SPACES OR LOW-VALUES
**Lines:** 159, 160

### BR002: Confirmation accepts Y/y (pay), N/n/spaces/low (no pay), others invalid
**Logic:** EVALUATE CONFIRMI sets CONF-PAY-YES or clears/errors
**Conditions:** CONFIRMI OF COBIL0AI = 'Y' OR 'y', CONFIRMI OF COBIL0AI = 'N' OR 'n' OR SPACES OR LOW-VALUES
**Lines:** 173, 176, 180, 186

### BR003: Balance >0 required for payment
**Logic:** Error if ACCT-CURR-BAL <= ZEROS
**Conditions:** ACCT-CURR-BAL <= ZEROS
**Lines:** 198, 200

### BR004: Sequential TRAN-ID via backward browse from HIGH-VALUES
**Logic:** STARTBR high, READPREV gets max, +1
**Conditions:** CONF-PAY-YES
**Lines:** 212, 217

### BR005: PF3 exits to caller or default (COMEN01C/COSGN00C)
**Logic:** Set CDEMO-TO-PROGRAM from CDEMO-FROM-PROGRAM or defaults, XCTL
**Conditions:** EIBAID = DFHPF3
**Lines:** 129, 135

### BR006: PF4 clears screen fields and redisplays
**Logic:** PERFORM CLEAR-CURRENT-SCREEN on DFHPF4
**Conditions:** EIBAID = DFHPF4
**Lines:** 137

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | CARDDEMO-COMMAREA and CDEMO-CB00-INFO for context/transaction selection | 63 |
| [COBIL00](../copybooks/COBIL00.md) | WORKING_STORAGE | BMS maps COBIL0AI/COBIL0AO for bill pay screen I/O | 74 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Titles/constants like CCDA-TITLE01/TITLE02 for screen headers | 76 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | CARD-XREF-RECORD for CXACAIX file | 77 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Messages like CCDA-MSG-INVALID-KEY for error display | 78 |
| [CVACT01Y](../copybooks/CVACT01Y.md) | WORKING_STORAGE | ACCOUNT-RECORD for ACCTDAT file | 80 |
| [CVACT03Y](../copybooks/CVACT03Y.md) | WORKING_STORAGE | Additional account-related structures (usage unclear from source) | 81 |
| [CVTRA05Y](../copybooks/CVTRA05Y.md) | WORKING_STORAGE | TRAN-RECORD for TRANSACT file | 82 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | EIBAID for aid key processing | 84 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | BMS constants DFHENTER/DFHPF3/DFHPF4/DFHGREEN | 85 |

## Data Flow

### Reads From
- **ACCTDAT**: ACCT-CURR-BAL, XREF-ACCT-ID
  (Lines: 170, 193)
- **CXACAIX**: XREF-CARD-NUM
  (Lines: 225)
- **TRANSACT**: TRAN-ID
  (Lines: 216)
- **COBIL0AI**: ACTIDINI, CONFIRMI
  (Lines: 169, 173)

### Writes To
- **TRANSACT**: TRAN-ID, TRAN-TYPE-CD, TRAN-AMT, TRAN-CARD-NUM, TRAN-ORIG-TS, TRAN-PROC-TS
  (Lines: 219, 220, 224, 225, 231)
- **ACCTDAT**: ACCT-CURR-BAL
  (Lines: 234)
- **COBIL0AO**: ERRMSGO, CURDATEO, CURTIMEO
  (Lines: 293, 332, 338)

### Transformations
- **TRAN-ID** → **WS-TRAN-ID-NUM**: Highest TRAN-ID from READPREV +1 for new ID
  (Lines: 216, 217)
- **ACCT-CURR-BAL** → **TRAN-AMT**: Full balance copied as payment amount
  (Lines: 224)
- **ACCT-CURR-BAL** → **CURBALI OF COBIL0AI**: Balance to screen via WS-CURR-BAL
  (Lines: 193, 194)
- **WS-ABS-TIME** → **WS-TIMESTAMP**: ASKTIME/FORMATTIME to YYYY-MM-DD-HH:MM:SS.000000
  (Lines: 251, 264)
- **ACCT-CURR-BAL** → **ACCT-CURR-BAL**: ACCT-CURR-BAL -= TRAN-AMT (to zero)
  (Lines: 234)
- **WS-TIMESTAMP** → **TRAN-ORIG-TS**: Formatted timestamp copied to transaction timestamps
  (Lines: 231)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Main: init flags, handle entry/reentry, aid keys, map send/receive
- Calls: RETURN-TO-PREV-SCREEN, PROCESS-ENTER-KEY, SEND-BILLPAY-SCREEN, RECEIVE-BILLPAY-SCREEN, CLEAR-CURRENT-SCREEN
- Lines: 99-148

### PROCESS-ENTER-KEY
**Purpose:** Validate/process payment: read files, gen txn, write/update if confirmed
- Called by: MAIN-PARA
- Calls: SEND-BILLPAY-SCREEN, CLEAR-CURRENT-SCREEN, READ-ACCTDAT-FILE, READ-CXACAIX-FILE, STARTBR-TRANSACT-FILE, READPREV-TRANSACT-FILE, ENDBR-TRANSACT-FILE, GET-CURRENT-TIMESTAMP, WRITE-TRANSACT-FILE, UPDATE-ACCTDAT-FILE
- Lines: 154-243

### GET-CURRENT-TIMESTAMP
**Purpose:** Format current timestamp into WS-TIMESTAMP
- Called by: PROCESS-ENTER-KEY
- Lines: 249-267

### RETURN-TO-PREV-SCREEN
**Purpose:** XCTL to prior program
- Called by: MAIN-PARA
- Lines: 273-283

### SEND-BILLPAY-SCREEN
**Purpose:** Populate/send map with header/message
- Called by: MAIN-PARA, PROCESS-ENTER-KEY, READ-ACCTDAT-FILE, UPDATE-ACCTDAT-FILE, READ-CXACAIX-FILE, STARTBR-TRANSACT-FILE, READPREV-TRANSACT-FILE, WRITE-TRANSACT-FILE, CLEAR-CURRENT-SCREEN
- Calls: POPULATE-HEADER-INFO
- Lines: 289-300

### RECEIVE-BILLPAY-SCREEN
**Purpose:** Receive map into COBIL0AI
- Called by: MAIN-PARA
- Lines: 306-313

### POPULATE-HEADER-INFO
**Purpose:** Set screen header date/time/titles/tranid
- Called by: SEND-BILLPAY-SCREEN
- Lines: 319-338

### READ-ACCTDAT-FILE
**Purpose:** READ UPDATE ACCTDAT by ACCT-ID
- Called by: PROCESS-ENTER-KEY
- Calls: SEND-BILLPAY-SCREEN
- Lines: 343-371

### UPDATE-ACCTDAT-FILE
**Purpose:** REWRITE ACCTDAT
- Called by: PROCESS-ENTER-KEY
- Calls: SEND-BILLPAY-SCREEN
- Lines: 377-402

### READ-CXACAIX-FILE
**Purpose:** READ CXACAIX by XREF-ACCT-ID
- Called by: PROCESS-ENTER-KEY
- Calls: SEND-BILLPAY-SCREEN
- Lines: 408-435

### STARTBR-TRANSACT-FILE
**Purpose:** STARTBR backward TRANSACT high key
- Called by: PROCESS-ENTER-KEY
- Calls: SEND-BILLPAY-SCREEN
- Lines: 441-466

### READPREV-TRANSACT-FILE
**Purpose:** READPREV TRANSACT for max ID
- Called by: PROCESS-ENTER-KEY
- Calls: SEND-BILLPAY-SCREEN
- Lines: 472-495

### ENDBR-TRANSACT-FILE
**Purpose:** ENDBR TRANSACT
- Called by: PROCESS-ENTER-KEY
- Lines: 501-504

### WRITE-TRANSACT-FILE
**Purpose:** WRITE new TRANSACT, success msg or error
- Called by: PROCESS-ENTER-KEY
- Calls: INITIALIZE-ALL-FIELDS, SEND-BILLPAY-SCREEN
- Lines: 510-546

### CLEAR-CURRENT-SCREEN
**Purpose:** Clear fields and resend screen
- Called by: MAIN-PARA, PROCESS-ENTER-KEY
- Calls: INITIALIZE-ALL-FIELDS, SEND-BILLPAY-SCREEN
- Lines: 552-555

### INITIALIZE-ALL-FIELDS
**Purpose:** Reset screen inputs and message
- Called by: WRITE-TRANSACT-FILE, CLEAR-CURRENT-SCREEN
- Lines: 560-565

## Error Handling

- **READ ACCTDAT NOTFND/OTHER:** ERR-FLG 'Y', msg, cursor ACTIDINL, SEND screen
  (Lines: 356, 363, 368)
- **REWRITE ACCTDAT NOTFND/OTHER:** ERR-FLG 'Y', msg, cursor ACTIDINL, SEND screen
  (Lines: 387, 391, 396)
- **READ CXACAIX NOTFND/OTHER:** ERR-FLG 'Y', msg, cursor ACTIDINL, SEND screen
  (Lines: 420, 424, 429)
- **STARTBR TRANSACT NOTFND/OTHER:** ERR-FLG 'Y', msg, cursor ACTIDINL, SEND screen
  (Lines: 451, 455, 460)
- **READPREV TRANSACT OTHER (ENDFILE sets ZEROS TRAN-ID):** ERR-FLG 'Y', msg, cursor ACTIDINL, SEND screen
  (Lines: 484, 491)
- **WRITE TRANSACT DUPKEY/DUPREC/OTHER:** ERR-FLG 'Y', msg, cursor ACTIDINL, SEND screen
  (Lines: 522, 534, 540)
- **ACTIDINI empty/low:** ERR-FLG 'Y', msg, cursor ACTIDINL, SEND
  (Lines: 159)
- **Balance <=0:** ERR-FLG 'Y', msg, cursor ACTIDINL, SEND
  (Lines: 198)
- **Invalid CONFIRMI:** ERR-FLG 'Y', msg, cursor CONFIRML, SEND
  (Lines: 186)
- **Invalid EIBAID:** ERR-FLG 'Y', invalid key msg, SEND
  (Lines: 139)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| RETURN | TRANSID(WS-TRANID) | Return to CICS with CB00 and commarea | 146 |
| ASKTIME | ABSTIME(WS-ABS-TIME) | Get current absolute time | 251 |
| FORMATTIME | ABSTIME(WS-ABS-TIME) | Format abstime to date/time strings | 255 |
| SEND | MAP('COBIL0A') MAPSET('COBIL00') | Send bill pay screen ERASE CURSOR | 295 |
| RECEIVE | MAP('COBIL0A') MAPSET('COBIL00') | Receive screen into COBIL0AI | 308 |
| READ | DATASET(WS-ACCTDAT-FILE) | Read ACCTDAT UPDATE by ACCT-ID | 345 |
| REWRITE | DATASET(WS-ACCTDAT-FILE) | Update ACCTDAT record | 379 |
| READ | DATASET(WS-CXACAIX-FILE) | Read CXACAIX by XREF-ACCT-ID | 410 |
| STARTBR | DATASET(WS-TRANSACT-FILE) | Backward browse TRANSACT from high TRAN-ID | 443 |
| READPREV | DATASET(WS-TRANSACT-FILE) | Read previous TRANSACT record | 474 |
| ENDBR | DATASET(WS-TRANSACT-FILE) | End backward browse | 503 |
| WRITE | DATASET(WS-TRANSACT-FILE) | Write new transaction record | 512 |
| XCTL | PROGRAM(CDEMO-TO-PROGRAM) | Transfer to previous program | 282 |

## Open Questions

- **Precise layouts/fields in copybooks (e.g., CVACT01Y ACCOUNT-RECORD details, CVACT03Y usage)**
  - Context: Inferred from usage but full structures unknown without copybook source
  - Suggestion: Analyze referenced copybooks
- **Programs calling COBIL00C via XCTL**
  - Context: Inferred from commarea CDEMO-FROM-PROGRAM but not explicit
  - Suggestion: Review transaction flow/callers like COSGN00C

---
*Generated by War Rig WAR_RIG*