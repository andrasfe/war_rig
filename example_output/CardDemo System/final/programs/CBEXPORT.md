# CBEXPORT

**File:** CBEXPORT.cbl
**Type:** COBOL
**Status:** FinalStatus.VALHALLA
**Iterations:** 2
**Analyzed:** 2026-01-18 17:35:36.076535

## Purpose

Batch COBOL program that reads all records sequentially from five indexed input files (customers, accounts, xrefs, transactions, cards) until EOF and writes them as fixed 500-character multi-record export file with type-specific layouts ('C','A','X','T','D'). Adds common header fields including record type, formatted current timestamp, incrementing sequence number, branch ID '0001', region 'NORTH'; maps input fields directly; tracks and displays export statistics.

**Business Context:** CardDemo branch migration: exports normalized data files into single multi-record format for data migration to new branch
**Program Type:** BATCH
**Citations:** Lines 5, 6, 7, 8, 27, 28, 29, 30, 149, 243, 312, 376, 431, 496

## Inputs

### CUSTOMER-INPUT
- **Type:** FILE_VSAM
- **Description:** Indexed sequential VSAM customer master file containing customer profiles: ID, names, addresses, phones, SSN, govt ID, DOB, EFT account ID, primary card holder ind, FICO score
- **Copybook:** [CVCUS01Y](../copybooks/CVCUS01Y.md)
- **Lines:** 260

### ACCOUNT-INPUT
- **Type:** FILE_VSAM
- **Description:** Indexed sequential VSAM account master file: account ID, active status, current balance, credit/cash limits, open/expiration/reissue dates, current cycle credit/debit, address ZIP, group ID
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 329

### XREF-INPUT
- **Type:** FILE_VSAM
- **Description:** Indexed sequential VSAM cross-reference file linking card number to customer ID and account ID
- **Copybook:** [CVACT03Y](../copybooks/CVACT03Y.md)
- **Lines:** 393

### TRANSACTION-INPUT
- **Type:** FILE_VSAM
- **Description:** Indexed sequential VSAM transaction file: transaction ID, type/cat codes, source, description, amount, merchant ID/name/city/ZIP, card number, original/process timestamps
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 448

### CARD-INPUT
- **Type:** FILE_VSAM
- **Description:** Indexed sequential VSAM card master file: card number, account ID, CVV code, embossed name, expiration date, active status
- **Copybook:** [CVACT02Y](../copybooks/CVACT02Y.md)
- **Lines:** 513

## Outputs

### EXPORT-OUTPUT
- **Type:** FILE_VSAM
- **Description:** Indexed sequential VSAM output file of fixed 500-char records; multi-record types 'C'(customer), 'A'(account), 'X'(xref), 'T'(transaction), 'D'(card); common prefix: rec-type, timestamp(YYYY-MM-DD HH:MM:SS.00), sequence num, branch '0001', region 'NORTH'; followed by type-specific fields directly mapped from inputs
- **Copybook:** [CVEXPORT](../copybooks/CVEXPORT.md)
- **Lines:** 301, 364, 419, 484, 542

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CEE3ABD](./CEE3ABD.md) | STATIC_CALL | Language Environment abend routine to terminate program abnormally | 579 |

## Business Rules

### BR001: Export every input record without filtering or selection criteria; process all sequentially from start to EOF
**Logic:** Initial READ then PERFORM UNTIL <FILE>-EOF alternating CREATE-EXP-REC and READ
**Conditions:** WS-CUSTOMER-EOF, WS-ACCOUNT-EOF, WS-XREF-EOF, WS-TRANSACTION-EOF, WS-CARD-EOF
**Lines:** 249, 318, 382, 437, 502

### BR002: Populate common export header for every record: type-specific rec-type ('C/A/X/T/D'), current formatted timestamp, incrementing sequence counter starting from 0, fixed branch '0001', region 'NORTH'
**Logic:** INITIALIZE EXPORT-RECORD then MOVE literals/computed values before input field MOVEs; ADD 1 TO WS-SEQUENCE-COUNTER per record
**Conditions:** Always executed in each CREATE-*-EXP-REC paragraph
**Lines:** 274, 275, 276, 278, 279, 343, 344, 345, 347, 348, 406, 407, 409, 411, 412, 462, 463, 464, 466, 467, 527, 528, 529, 531, 532

### BR003: Strict file error handling: abend immediately on any non-zero/non-EOF file status during OPEN, READ (non-EOF), or WRITE
**Logic:** IF NOT <FILE>-OK [AND NOT EOF for READ] then DISPLAY error status and PERFORM 9999-ABEND-PROGRAM
**Conditions:** NOT WS-*-OK, NOT WS-*-EOF (for READ)
**Lines:** 201, 262, 303, 208, 331, 366, 215, 395, 421, 222, 450, 486, 229, 515, 544, 236

### BR004: Generate formatted current timestamp for export headers: date YYYY-MM-DD, time HH:MM:SS.00
**Logic:** ACCEPT DATE/TIME YYYYMMDD/HHMMSSHH then STRING with delimiters
**Conditions:** Executed once in 1050-GENERATE-TIMESTAMP
**Lines:** 175, 176, 179, 184, 191

### BR005: Track and report export statistics: per-type record counts and grand total, displayed at start/process/end
**Logic:** Increment counters ADD 1 TO WS-*-RECORDS-EXPORTED and WS-TOTAL-RECORDS-EXPORTED after each successful WRITE; DISPLAY in loops and FINALIZE
**Conditions:** After each successful export record write
**Lines:** 309, 372, 427, 492, 550, 255, 324, 388, 443, 508, 564, 566, 568, 569, 571, 573

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVCUS01Y](../copybooks/CVCUS01Y.md) | FILE_SECTION | Customer input record layout for CUSTOMER-INPUT FD | 75 |
| [CVACT01Y](../copybooks/CVACT01Y.md) | FILE_SECTION | Account input record layout for ACCOUNT-INPUT FD | 78 |
| [CVACT03Y](../copybooks/CVACT03Y.md) | FILE_SECTION | XREF input record layout for XREF-INPUT FD | 81 |
| [CVTRA05Y](../copybooks/CVTRA05Y.md) | FILE_SECTION | Transaction input record layout for TRANSACTION-INPUT FD | 84 |
| [CVACT02Y](../copybooks/CVACT02Y.md) | FILE_SECTION | Card input record layout for CARD-INPUT FD | 87 |
| [CVEXPORT](../copybooks/CVEXPORT.md) | WORKING_STORAGE | Export record layout, file status areas, control vars, timestamps, statistics counters in WORKING-STORAGE | 96 |

## Data Flow

### Reads From
- **CUSTOMER-INPUT**: CUST-ID, CUST-FIRST-NAME, CUST-MIDDLE-NAME, CUST-LAST-NAME, CUST-ADDR-LINE-1, CUST-ADDR-LINE-2, CUST-ADDR-LINE-3, CUST-ADDR-STATE-CD, CUST-ADDR-COUNTRY-CD, CUST-ADDR-ZIP, CUST-PHONE-NUM-1, CUST-PHONE-NUM-2, CUST-SSN, CUST-GOVT-ISSUED-ID, CUST-DOB-YYYY-MM-DD, CUST-EFT-ACCOUNT-ID, CUST-PRI-CARD-HOLDER-IND, CUST-FICO-CREDIT-SCORE
  (Lines: 260, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300)
- **ACCOUNT-INPUT**: ACCT-ID, ACCT-ACTIVE-STATUS, ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, ACCT-OPEN-DATE, ACCT-EXPIRAION-DATE, ACCT-REISSUE-DATE, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT, ACCT-ADDR-ZIP, ACCT-GROUP-ID
  (Lines: 329, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362)
- **XREF-INPUT**: XREF-CARD-NUM, XREF-CUST-ID, XREF-ACCT-ID
  (Lines: 393, 415, 416, 417)
- **TRANSACTION-INPUT**: TRAN-ID, TRAN-TYPE-CD, TRAN-CAT-CD, TRAN-SOURCE, TRAN-DESC, TRAN-AMT, TRAN-MERCHANT-ID, TRAN-MERCHANT-NAME, TRAN-MERCHANT-CITY, TRAN-MERCHANT-ZIP, TRAN-CARD-NUM, TRAN-ORIG-TS, TRAN-PROC-TS
  (Lines: 448, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479, 480, 481, 482, 483)
- **CARD-INPUT**: CARD-NUM, CARD-ACCT-ID, CARD-CVV-CD, CARD-EMBOSSED-NAME, CARD-EXPIRAION-DATE, CARD-ACTIVE-STATUS
  (Lines: 513, 535, 536, 537, 538, 539, 540, 541)

### Writes To
- **EXPORT-OUTPUT**: EXPORT-REC-TYPE, EXPORT-TIMESTAMP, EXPORT-SEQUENCE-NUM, EXPORT-BRANCH-ID, EXPORT-REGION-CODE, EXP-CUST-*, EXP-ACCT-*, EXP-XREF-*, EXP-TRAN-*, EXP-CARD-*
  (Lines: 301, 364, 419, 484, 542)

### Transformations
- **WS-CURRENT-DATE** → **WS-EXPORT-DATE**: ACCEPT FROM DATE YYYYMMDD then STRING YYYY-MM-DD delimited by '-'
  (Lines: 175, 179)
- **WS-CURRENT-TIME** → **WS-EXPORT-TIME**: ACCEPT FROM TIME then STRING HH:MM:SS delimited by ':'
  (Lines: 176, 184)
- **WS-EXPORT-DATE WS-EXPORT-TIME** → **WS-FORMATTED-TIMESTAMP**: STRING date space time .00 into 26-char timestamp
  (Lines: 191)
- **CUST-ID** → **EXP-CUST-ID**: Direct MOVE from input record
  (Lines: 282)
- **ACCT-ID** → **EXP-ACCT-ID**: Direct MOVE from input record
  (Lines: 351)
- **TRAN-ID** → **EXP-TRAN-ID**: Direct MOVE from input record
  (Lines: 470)

## Key Paragraphs

### 0000-MAIN-PROCESSING
**Purpose:** Orchestrates entire process: initialize, export each entity type in sequence, finalize, GOBACK
- Calls: 1000-INITIALIZE, 2000-EXPORT-CUSTOMERS, 3000-EXPORT-ACCOUNTS, 4000-EXPORT-XREFS, 5000-EXPORT-TRANSACTIONS, 5500-EXPORT-CARDS, 6000-FINALIZE
- Lines: 149-158

### 1000-INITIALIZE
**Purpose:** Display start message, generate timestamp, open all input/output files with error checks
- Called by: 0000-MAIN-PROCESSING
- Calls: 1050-GENERATE-TIMESTAMP, 1100-OPEN-FILES
- Lines: 161-170

### 1050-GENERATE-TIMESTAMP
**Purpose:** ACCEPT current DATE/TIME, format into export-ready strings (YYYY-MM-DD, HH:MM:SS, full timestamp)
- Called by: 1000-INITIALIZE
- Lines: 172-195

### 1100-OPEN-FILES
**Purpose:** OPEN INPUT all five files and OUTPUT EXPORT-OUTPUT; abend on any open failure
- Called by: 1000-INITIALIZE
- Calls: 9999-ABEND-PROGRAM
- Lines: 198-240

### 2000-EXPORT-CUSTOMERS
**Purpose:** Initial read then loop until EOF: create customer export rec then read next
- Called by: 0000-MAIN-PROCESSING
- Calls: 2100-READ-CUSTOMER-RECORD, 2200-CREATE-CUSTOMER-EXP-REC
- Lines: 243-255

### 2100-READ-CUSTOMER-RECORD
**Purpose:** READ NEXT sequential; abend if status not OK or EOF
- Called by: 2000-EXPORT-CUSTOMERS
- Calls: 9999-ABEND-PROGRAM
- Lines: 258-266

### 2200-CREATE-CUSTOMER-EXP-REC
**Purpose:** Initialize, set headers, MOVE all customer fields, WRITE, check status/abend/increment counters
- Called by: 2000-EXPORT-CUSTOMERS
- Calls: 9999-ABEND-PROGRAM
- Lines: 269-310

### 3000-EXPORT-ACCOUNTS
**Purpose:** Initial read then loop until EOF: create account export rec then read next; display count
- Called by: 0000-MAIN-PROCESSING
- Calls: 3100-READ-ACCOUNT-RECORD, 3200-CREATE-ACCOUNT-EXP-REC
- Lines: 312-324

### 4000-EXPORT-XREFS
**Purpose:** Initial read then loop until EOF: create xref export rec then read next; display count
- Called by: 0000-MAIN-PROCESSING
- Calls: 4100-READ-XREF-RECORD, 4200-CREATE-XREF-EXPORT-RECORD
- Lines: 376-388

### 5000-EXPORT-TRANSACTIONS
**Purpose:** Initial read then loop until EOF: create transaction export rec then read next; display count
- Called by: 0000-MAIN-PROCESSING
- Calls: 5100-READ-TRANSACTION-RECORD, 5200-CREATE-TRAN-EXP-REC
- Lines: 431-443

### 5500-EXPORT-CARDS
**Purpose:** Initial read then loop until EOF: create card export rec then read next; display count
- Called by: 0000-MAIN-PROCESSING
- Calls: 5600-READ-CARD-RECORD, 5700-CREATE-CARD-EXPORT-RECORD
- Lines: 496-508

### 6000-FINALIZE
**Purpose:** CLOSE all files; display completion message and all statistics
- Called by: 0000-MAIN-PROCESSING
- Lines: 554-573

### 9999-ABEND-PROGRAM
**Purpose:** Display 'ABENDING PROGRAM' and static CALL CEE3ABD to terminate
- Called by: 1100-OPEN-FILES, 2100-READ-CUSTOMER-RECORD, 2200-CREATE-CUSTOMER-EXP-REC, 3000-EXPORT-ACCOUNTS* similar
- Lines: 576-579

## Error Handling

- **OPEN INPUT/OUTPUT file status NOT '00':** DISPLAY error with file name and status then PERFORM 9999-ABEND-PROGRAM
  (Lines: 202, 209, 216, 223, 230, 237)
- **READ file status NOT '00' AND NOT '10' (EOF):** DISPLAY error with file name and status then PERFORM 9999-ABEND-PROGRAM
  (Lines: 263, 332, 396, 451, 516)
- **WRITE EXPORT-OUTPUT-RECORD status NOT '00':** DISPLAY error with status then PERFORM 9999-ABEND-PROGRAM
  (Lines: 304, 367, 422, 487, 545)

## Open Questions

- **Precise layouts and data types of all fields in copybooks (CVCUS01Y, CVACT01Y, etc., CVEXPORT)**
  - Context: Field names and usage clear from MOVEs, but exact PIC clauses, OCCURS, REDEFINES unknown without copybook source
  - Suggestion: Obtain and analyze copybook source files

---
*Generated by War Rig WAR_RIG*