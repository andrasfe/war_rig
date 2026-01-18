# CBTRN02C

**File:** CBTRN02C.cbl
**Type:** COBOL
**Status:** FinalStatus.VALHALLA
**Iterations:** 2
**Analyzed:** 2026-01-18 17:05:17.993283

## Purpose

Batch COBOL program reads sequential daily transaction file (DALYTRAN-FILE), validates each transaction by looking up card number in XREF-FILE, confirming account existence/credit limit/expiration in ACCOUNT-FILE. Posts valid transactions by copying/augmenting to indexed TRANSACT-FILE, updating account current balance/cycle credit-or-debit and transaction category balances in TCATBAL-FILE; rejects invalid ones to DALYREJS-FILE with reason codes.

**Business Context:** CardDemo application: processes credit card daily transactions with validation against cross-ref, account limits, expiration before posting updates to balances.
**Program Type:** BATCH
**Citations:** Lines 3, 4, 5, 202, 210, 212, 424

## Inputs

### DALYTRAN-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Daily input transactions with ID (PIC X(16)), customer data (PIC X(334)) including card num, amt, type-cd, cat-cd, merchant details, orig-ts.
- **Copybook:** [CVTRA06Y](../copybooks/CVTRA06Y.md)
- **Lines:** 29, 346

### XREF-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed cross-ref from card num (PIC X(16)) to acct ID and data (PIC X(34)).
- **Copybook:** [CVACT03Y](../copybooks/CVACT03Y.md)
- **Lines:** 40, 383

### ACCOUNT-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed account master by acct ID (PIC 9(11)), with data incl. curr-bal, curr-cyc-credit/debit, credit-limit, expiraion-date.
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 51, 395

### TCATBAL-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed transaction category balances by composite key acct-ID/type-cd/cat-cd.
- **Copybook:** [CVTRA01Y](../copybooks/CVTRA01Y.md)
- **Lines:** 57, 473

## Outputs

### TRANSACT-FILE
- **Type:** FILE_VSAM
- **Description:** Posted transactions copied from input + proc-ts, indexed by trans-ID.
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 34, 564

### DALYREJS-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Rejected input transactions (350 bytes reject-tran-data from DALYTRAN-RECORD) + 80-byte validation-trailer with fail-reason code/desc; inline FD layout.
- **Lines:** 46, 447, 448

### ACCOUNT-FILE
- **Type:** FILE_VSAM
- **Description:** Updated via REWRITE: incremented curr-bal, conditional curr-cyc-credit/debit.
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 554

### TCATBAL-FILE
- **Type:** FILE_VSAM
- **Description:** New (WRITE) or updated (REWRITE) category balance += trans-amt.
- **Copybook:** [CVTRA01Y](../copybooks/CVTRA01Y.md)
- **Lines:** 510, 528

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CEE3ABD](./CEE3ABD.md) | STATIC_CALL | Abend program on errors with code 999. | 711 |

## Business Rules

### BR001: Reject if card number invalid/not found in XREF-FILE.
**Logic:** READ XREF-FILE INVALID KEY clause sets reason=100.
**Conditions:** INVALID KEY on READ XREF-FILE
**Lines:** 384, 385

### BR002: Reject if account record not found in ACCOUNT-FILE.
**Logic:** READ ACCOUNT-FILE INVALID KEY sets reason=101.
**Conditions:** INVALID KEY on READ ACCOUNT-FILE
**Lines:** 396, 397

### BR003: Reject if projected cycle bal exceeds credit limit.
**Logic:** WS-TEMP-BAL = cyc-credit - cyc-debit + trans-amt > credit-limit sets reason=102.
**Conditions:** ACCT-CREDIT-LIMIT < WS-TEMP-BAL
**Lines:** 403, 407, 410

### BR004: Reject if trans received after acct expiration.
**Logic:** ACCT-EXPIRAION-DATE < DALYTRAN-ORIG-TS(1:10) sets reason=103.
**Conditions:** ACCT-EXPIRAION-DATE < DALYTRAN-ORIG-TS(1:10)
**Lines:** 414, 417

### BR005: Increment cycle credit if trans-amt >=0 else cycle debit.
**Logic:** Conditional ADD DALYTRAN-AMT to ACCT-CURR-CYC-CREDIT/DEBIT based on amt sign.
**Conditions:** DALYTRAN-AMT >= 0
**Lines:** 548, 549, 551

### BR006: Create new TCATBAL rec if READ INVALID KEY (status '23'), else update existing.
**Logic:** READ sets WS-CREATE-TRANCAT-REC='Y' on INVALID KEY; then conditional WRITE/REWRITE.
**Conditions:** TCATBALF-STATUS = '23'
**Lines:** 473, 476, 478, 481

### BR007: Set cond code 4 if any rejects occurred.
**Logic:** If WS-REJECT-COUNT > 0 MOVE 4 TO RETURN-CODE.
**Conditions:** WS-REJECT-COUNT > 0
**Lines:** 229, 230

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVTRA06Y](../copybooks/CVTRA06Y.md) | WORKING_STORAGE | DALYTRAN-RECORD layout for input daily trans. | 102 |
| [CVTRA05Y](../copybooks/CVTRA05Y.md) | WORKING_STORAGE | TRAN-RECORD layout for output transact file. | 107 |
| [CVACT03Y](../copybooks/CVACT03Y.md) | WORKING_STORAGE | CARD-XREF-RECORD layout for XREF-FILE. | 112 |
| [CVACT01Y](../copybooks/CVACT01Y.md) | WORKING_STORAGE | ACCOUNT-RECORD layout for ACCOUNT-FILE. | 121 |
| [CVTRA01Y](../copybooks/CVTRA01Y.md) | WORKING_STORAGE | TRAN-CAT-BAL-RECORD layout for TCATBAL-FILE. | 126 |

## Data Flow

### Reads From
- **DALYTRAN-FILE**: DALYTRAN-ID, DALYTRAN-CARD-NUM, DALYTRAN-AMT, DALYTRAN-TYPE-CD, DALYTRAN-CAT-CD, DALYTRAN-ORIG-TS, DALYTRAN-SOURCE, DALYTRAN-DESC, DALYTRAN-MERCHANT-ID, DALYTRAN-MERCHANT-NAME, DALYTRAN-MERCHANT-CITY, DALYTRAN-MERCHANT-ZIP
  (Lines: 346, 381, 425, 426, 427, 436)
- **XREF-FILE**: XREF-ACCT-ID
  (Lines: 394)
- **ACCOUNT-FILE**: ACCT-CREDIT-LIMIT, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT, ACCT-EXPIRAION-DATE, ACCT-CURR-BAL
  (Lines: 401, 403, 414, 547)
- **TCATBAL-FILE**: TRAN-CAT-BAL
  (Lines: 527)

### Writes To
- **TRANSACT-FILE**: TRAN-ID, TRAN-TYPE-CD, TRAN-CAT-CD, TRAN-AMT, TRAN-PROC-TS
  (Lines: 425, 430, 438)
- **DALYREJS-FILE**: REJECT-TRAN-DATA, VALIDATION-TRAILER
  (Lines: 447, 448)
- **ACCOUNT-FILE**: ACCT-CURR-BAL, ACCT-CURR-CYC-CREDIT or ACCT-CURR-CYC-DEBIT (amt>=0 ? credit : debit)
  (Lines: 547, 549, 551)
- **TCATBAL-FILE**: TRAN-CAT-BAL, TRANCAT-ACCT-ID, TRANCAT-TYPE-CD, TRANCAT-CD
  (Lines: 505, 508, 527)

### Transformations
- **DALYTRAN-ORIG-TS** → **TRAN-PROC-TS**: Copy orig-ts; overwrite with current DB2-format ts from FUNCTION CURRENT-DATE.
  (Lines: 436, 437, 693)
- **DALYTRAN-AMT** → **ACCT-CURR-BAL**: ADD to account current balance.
  (Lines: 547)
- **DALYTRAN-AMT** → **TRAN-CAT-BAL**: ADD to category balance (new or existing rec).
  (Lines: 508, 527)
- **ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT, DALYTRAN-AMT** → **WS-TEMP-BAL**: Compute projected bal = credit - debit + amt for limit check.
  (Lines: 403)
- **DALYTRAN-AMT** → **ACCT-CURR-CYC-CREDIT or ACCT-CURR-CYC-DEBIT**: If amt >=0 ADD to cycle-credit else ADD to cycle-debit.
  (Lines: 548)

## Key Paragraphs

### 0000-DALYTRAN-OPEN
**Purpose:** OPEN INPUT DALYTRAN-FILE, check status=00 else abend.
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 236-252

### 0100-TRANFILE-OPEN
**Purpose:** OPEN OUTPUT TRANSACT-FILE, check status=00 else abend.
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 254-270

### 0200-XREFFILE-OPEN
**Purpose:** OPEN INPUT XREF-FILE, check status=00 else abend.
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 273-289

### 0300-DALYREJS-OPEN
**Purpose:** OPEN OUTPUT DALYREJS-FILE, check status=00 else abend.
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 291-307

### 0400-ACCTFILE-OPEN
**Purpose:** OPEN I-O ACCOUNT-FILE, check status=00 else abend.
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 309-325

### 0500-TCATBALF-OPEN
**Purpose:** OPEN I-O TCATBAL-FILE, check status=00 else abend.
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 327-343

### 1000-DALYTRAN-GET-NEXT
**Purpose:** READ NEXT DALYTRAN-FILE, set EOF on '10', error else abend.
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 345-369

### 1500-VALIDATE-TRAN
**Purpose:** Chain XREF lookup then ACCT lookup/validations if prior OK.
- Calls: 1500-A-LOOKUP-XREF, 1500-B-LOOKUP-ACCT
- Lines: 370-378

### 2000-POST-TRANSACTION
**Purpose:** Populate/build output trans rec, timestamp, update TCATBAL/ACCT, write trans.
- Calls: Z-GET-DB2-FORMAT-TIMESTAMP, 2700-UPDATE-TCATBAL, 2800-UPDATE-ACCOUNT-REC, 2900-WRITE-TRANSACTION-FILE
- Lines: 424-444

### 2500-WRITE-REJECT-REC
**Purpose:** WRITE reject rec w/ trailer to DALYREJS-FILE, abend on error.
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 446-465

### 2700-UPDATE-TCATBAL
**Purpose:** READ TCATBAL by key, create if '23'/invalid else update bal.
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM, 2700-A-CREATE-TCATBAL-REC, 2700-B-UPDATE-TCATBAL-REC
- Lines: 467-501

### 9999-ABEND-PROGRAM
**Purpose:** DISPLAY abend msg, CALL CEE3ABD(999,0).
- Lines: 707-713

## Error Handling

- **File status <> '00' on OPEN INPUT/OUTPUT/I-O (all files):** DISPLAY error msg, PERFORM 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
  (Lines: 249, 267, 286, 304, 322, 340)
- **File status <> '00'/'10' on READ DALYTRAN-FILE:** DISPLAY error, 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
  (Lines: 365)
- **File status <> '00' on WRITE/REWRITE TCATBAL-FILE, ACCOUNT-FILE, TRANSACT-FILE, DALYREJS-FILE:** DISPLAY error, 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
  (Lines: 462, 491, 522, 540, 576, 595, 613, 632, 650, 668, 687)
- **File status <> '00' on CLOSE (all files):** DISPLAY error, 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
  (Lines: 595, 613, 632, 650, 668, 687)
- **TCATBALF-STATUS = '00' or '23' on READ TCATBAL-FILE:** OK: set create flag if '23', proceed to WRITE/REWRITE
  (Lines: 481)
- **DALYTRAN-STATUS = '10' on READ DALYTRAN-FILE:** Set END-OF-FILE='Y'
  (Lines: 351)
- **INVALID KEY on REWRITE ACCOUNT-FILE:** Set validation fail reason=109 (late, post-rewrite)
  (Lines: 556)

## Open Questions

- **Detailed field layouts in copybooks (e.g., exact positions of DALYTRAN-CARD-NUM, ACCT-CREDIT-LIMIT).**
  - Context: Fields referenced but copybook contents not in source.
  - Suggestion: Obtain/analyze copybook sources.
- **JCL/DD statements for file assignments and calling context.**
  - Context: Batch program; no info in source.
  - Suggestion: Review invoking JCL/PROC.
- **VSAM specifics (KSDS/ESDS) for indexed files.**
  - Context: ORGANIZATION INDEXED/ACCESS RANDOM implies VSAM but unconfirmed.
  - Suggestion: Check IDCAMS or catalog.

---
*Generated by War Rig WAR_RIG*