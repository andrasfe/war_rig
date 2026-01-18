# CBACT04C

**File:** CBACT04C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 2
**Analyzed:** 2026-01-18 15:51:54.171634

## Purpose

Batch program reads transaction category balance records sequentially from TCATBAL-FILE, groups by account ID, retrieves account details from ACCOUNT-FILE, cross-reference from XREF-FILE, and interest rates from DISCGRP-FILE. Computes and accumulates monthly interest per account using formula (balance * rate)/1200, writes interest transactions to TRANSACT-FILE using PARM-DATE, and updates account current balance via REWRITE.

**Business Context:** Card account interest calculation, posting interest transactions, and updating account balances in a batch process for CardDemo application.
**Program Type:** BATCH
**Citations:** Lines 4, 5, 28, 188, 325, 464

## Calling Context

**Linkage Section:** EXTERNAL-PARMS

## Inputs

### TCATBAL-FILE
- **Type:** FILE_VSAM
- **Description:** Transaction category balance records containing account ID, type code, category code, and balance data
- **Copybook:** [CVTRA01Y](../copybooks/CVTRA01Y.md)
- **Lines:** 28, 325

### XREF-FILE
- **Type:** FILE_VSAM
- **Description:** Cross-reference records linking card number, customer number, and account ID
- **Copybook:** [CVACT03Y](../copybooks/CVACT03Y.md)
- **Lines:** 34, 394

### ACCOUNT-FILE
- **Type:** FILE_VSAM
- **Description:** Account master records containing current balance and cycle credits/debits
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 41, 372

### DISCGRP-FILE
- **Type:** FILE_VSAM
- **Description:** Disclosure group records providing interest rates by account group, transaction type, and category
- **Copybook:** [CVTRA02Y](../copybooks/CVTRA02Y.md)
- **Lines:** 47, 416

### EXTERNAL-PARMS
- **Type:** PARAMETER
- **Description:** Input parameters including PARM-DATE used for transaction ID generation
- **Lines:** 176, 476

## Outputs

### TRANSACT-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Output transaction records for interest postings including transaction ID, amount, description, timestamps
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 53, 500

### ACCOUNT-FILE
- **Type:** FILE_VSAM
- **Description:** Updated account records with added total interest to current balance and reset cycle credits/debits
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 41, 356

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CEE3ABD](./CEE3ABD.md) | STATIC_CALL | Abend the program with user code 999 | 632 |

## Business Rules

### BR001: Accumulate interest only after processing all category balances for the same account ID before updating account
**Logic:** Tracks last account with WS-LAST-ACCT-NUM, calls UPDATE-ACCOUNT only on change or EOF
**Conditions:** IF TRANCAT-ACCT-ID NOT= WS-LAST-ACCT-NUM, IF WS-FIRST-TIME NOT = 'Y'
**Lines:** 194, 196

### BR002: Monthly interest = (category balance * interest rate) / 1200
**Logic:** Computes WS-MONTHLY-INT and adds to WS-TOTAL-INT if rate found
**Conditions:** IF DIS-INT-RATE NOT = 0
**Lines:** 464, 215

### BR003: Use default disclosure group 'DEFAULT' if specific group not found (status 23)
**Logic:** Performs 1200-A-GET-DEFAULT-INT-RATE on INVALID KEY or status 23
**Conditions:** IF DISCGRP-STATUS = '23'
**Lines:** 436, 438

### BR004: Transaction ID generated as PARM-DATE + sequential suffix starting from 0
**Logic:** STRING PARM-DATE WS-TRANID-SUFFIX into TRAN-ID, increment suffix each time
**Lines:** 476, 474

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVTRA01Y](../copybooks/CVTRA01Y.md) | WORKING_STORAGE | Defines transaction category balance record structure including TRAN-CAT-BAL-RECORD | 97 |
| [CVACT03Y](../copybooks/CVACT03Y.md) | WORKING_STORAGE | Defines cross-reference record structure including CARD-XREF-RECORD | 102 |
| [CVTRA02Y](../copybooks/CVTRA02Y.md) | WORKING_STORAGE | Defines disclosure group record structure including DIS-GROUP-RECORD | 107 |
| [CVACT01Y](../copybooks/CVACT01Y.md) | WORKING_STORAGE | Defines account record structure including ACCOUNT-RECORD | 112 |
| [CVTRA05Y](../copybooks/CVTRA05Y.md) | WORKING_STORAGE | Defines transaction output record structure including TRAN-RECORD | 117 |

## Data Flow

### Reads From
- **TCATBAL-FILE**: TRANCAT-ACCT-ID, TRANCAT-TYPE-CD, TRANCAT-CD, TRAN-CAT-BAL
  (Lines: 194, 210, 325)
- **ACCOUNT-FILE**: ACCT-CURR-BAL, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT, ACCT-GROUP-ID
  (Lines: 352, 210, 372)
- **XREF-FILE**: XREF-CARD-NUM
  (Lines: 495, 394)
- **DISCGRP-FILE**: DIS-INT-RATE
  (Lines: 215, 416)

### Writes To
- **TRANSACT-FILE**: TRAN-ID, TRAN-TYPE-CD, TRAN-CAT-CD, TRAN-AMT, TRAN-DESC, TRAN-CARD-NUM, TRAN-ORIG-TS, TRAN-PROC-TS
  (Lines: 476, 500)
- **ACCOUNT-FILE**: ACCT-CURR-BAL, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT
  (Lines: 352, 356)

### Transformations
- **TRAN-CAT-BAL** → **WS-MONTHLY-INT**: WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200 then added to WS-TOTAL-INT
  (Lines: 464)
- **PARM-DATE** → **TRAN-ID**: STRING PARM-DATE WS-TRANID-SUFFIX DELIMITED BY SIZE INTO TRAN-ID
  (Lines: 476)
- **WS-TOTAL-INT** → **ACCT-CURR-BAL**: ADD WS-TOTAL-INT TO ACCT-CURR-BAL
  (Lines: 352)
- **COBOL-TS** → **DB2-FORMAT-TS**: Format current date to DB2 timestamp string with dashes and dots
  (Lines: 614)

## Key Paragraphs

### 0000-TCATBALF-OPEN
**Purpose:** Opens TCATBAL-FILE for input and checks status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 234-249

### 0100-XREFFILE-OPEN
**Purpose:** Opens XREF-FILE for input and checks status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 252-267

### 0200-DISCGRP-OPEN
**Purpose:** Opens DISCGRP-FILE for input and checks status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 270-285

### 0300-ACCTFILE-OPEN
**Purpose:** Opens ACCOUNT-FILE for I-O and checks status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 289-304

### 0400-TRANFILE-OPEN
**Purpose:** Opens TRANSACT-FILE for output and checks status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 307-322

### 1000-TCATBALF-GET-NEXT
**Purpose:** Reads next sequential record from TCATBAL-FILE, sets EOF flag on status 10
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 325-347

### 1050-UPDATE-ACCOUNT
**Purpose:** Adds total interest to account balance, resets cycle fields, rewrites account record
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 350-369

### 1100-GET-ACCT-DATA
**Purpose:** Random read ACCOUNT-FILE by FD-ACCT-ID into ACCOUNT-RECORD
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 372-390

### 1110-GET-XREF-DATA
**Purpose:** Random read XREF-FILE by FD-XREF-ACCT-ID into CARD-XREF-RECORD
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 393-412

### 1200-GET-INTEREST-RATE
**Purpose:** Random read DISCGRP-FILE by key, fallback to default on status 23
- Calls: 1200-A-GET-DEFAULT-INT-RATE, 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 415-439

### 1300-COMPUTE-INTEREST
**Purpose:** Computes monthly interest and accumulates total, writes transaction
- Calls: 1300-B-WRITE-TX
- Lines: 462-469

### 1400-COMPUTE-FEES
**Purpose:** Placeholder for future fee computation (empty)
- Lines: 518-519

### 9000-TCATBALF-CLOSE
**Purpose:** Closes TCATBAL-FILE and checks status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 522-537

### 9999-ABEND-PROGRAM
**Purpose:** Calls CEE3ABD to abend program with code 999
- Lines: 628-632

### 9910-DISPLAY-IO-STATUS
**Purpose:** Displays file IO-STATUS in formatted numeric message
- Lines: 635-647

## Error Handling

- **File status not '00' on OPEN/READ/WRITE/REWRITE/CLOSE:** Display error message, PERFORM 9910-DISPLAY-IO-STATUS, PERFORM 9999-ABEND-PROGRAM
  (Lines: 245, 263, 281, 300, 318, 342, 365, 386, 408, 431, 455, 510, 533)
- **TCATBALF-STATUS = '10':** Set APPL-EOF and END-OF-FILE = 'Y'
  (Lines: 330)
- **DISCGRP-STATUS = '23':** Use default group and re-read
  (Lines: 436)

## Open Questions

- **Exact layout and field definitions in copybooks (e.g., TRAN-CAT-BAL, ACCT-CURR-BAL, DIS-INT-RATE)**
  - Context: Copybooks CVTRA01Y etc. are referenced but contents not provided in source
  - Suggestion: Analyze copybook source files
- **Implementation of 1400-COMPUTE-FEES**
  - Context: Currently empty with comment 'To be implemented'
  - Suggestion: Check for future updates or related programs
- **Business meaning of /1200 in interest formula**
  - Context: Likely annual rate to monthly (rate/100 /12), but not explicitly stated
  - Suggestion: Review business requirements or DISCGRP-FILE rate definition

---
*Generated by War Rig WAR_RIG*