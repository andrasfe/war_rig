# CBSTM03A

**File:** CBSTM03A.CBL
**Type:** COBOL
**Status:** FinalStatus.VALHALLA
**Iterations:** 2
**Analyzed:** 2026-01-18 18:02:21.124408

## Purpose

Generates customer account statements in plain text and HTML formats for reporting or billing, using cross-reference data to link transactions to customers and accounts.

**Business Context:** Part of CardDemo application to produce statements from transaction history.
**Program Type:** BATCH
**Citations:** Lines 5, 6, 7, 8, 9, 26, 316

## Calling Context

**Linkage Section:** PSA-BLOCK, TCB-BLOCK, TIOT-BLOCK, TIOT-ENTRY

## Inputs

### TRNXFILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Transaction records containing card number, transaction ID, description, amount, read sequentially and grouped into memory table by card.
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 730, 746, 833, 839

### XREFFILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Cross-reference records linking card numbers to customer and account IDs, read sequentially to drive statement generation.
- **Copybook:** [COSTM01](../copybooks/COSTM01.md)
- **Lines:** 345, 347, 351, 364

### CUSTFILE
- **Type:** FILE_VSAM
- **Description:** Customer master records with name, address, FICO score, read by customer ID key.
- **Copybook:** [CUSTREC](../copybooks/CUSTREC.md)
- **Lines:** 368, 370, 372, 377, 388

### ACCTFILE
- **Type:** FILE_VSAM
- **Description:** Account records with ID and current balance, read by account ID key.
- **Copybook:** [CVACT03Y](../copybooks/CVACT03Y.md)
- **Lines:** 392, 394, 396, 401, 412

## Outputs

### STMT-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Plain text account statements with fixed format lines for header, customer details, account info, transaction list, and totals.
- **Lines:** 293, 460, 487, 489, 679, 435

### HTML-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** HTML account statements using tables with styled rows for header, basic details, and transaction rows.
- **Lines:** 293, 461, 486, 508, 558, 681

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CBSTM03B](./CBSTM03B.md) | STATIC_CALL | Universal file handler for OPEN, CLOSE, READ, READ key operations on all input files via DDNAME. | 351 |
| [CEE3ABD](./CEE3ABD.md) | STATIC_CALL | Language Environment abend routine called on all errors. | 923 |

## Business Rules

### BR001: Pre-load all transactions into 2D table WS-TRNX-TABLE (51 cards x 10 trans max) grouped by card number before processing statements.
**Logic:** Initial sequential read loop in 8500-READTRNX-READ detects card changes via WS-SAVE-CARD to increment CR-CNT/TR-CNT and store.
**Conditions:** WS-SAVE-CARD = TRNX-CARD-NUM, WS-M03B-RC = '00'
**Lines:** 756, 757, 819, 822, 827, 850

### BR002: Generate one statement per XREFFILE record by matching card to customer/account and listing only matching transactions from table.
**Logic:** Main loop reads XREFFILE seq, key reads CUST/ACCT, searches table for XREF-CARD-NUM matches, sums/writes trans.
**Conditions:** END-OF-FILE = 'N', XREF-CARD-NUM = WS-CARD-NUM (CR-JMP)
**Lines:** 317, 319, 321, 322, 417, 420

### BR003: Dynamically sequence initial file opens using ALTER verb based on WS-FL-DD flag.
**Logic:** EVALUATE sets ALTER target for 8100-FILE-OPEN: TRNXFILE (load table) -> XREFFILE -> CUSTFILE -> ACCTFILE -> MAINLINE.
**Conditions:** WS-FL-DD = 'TRNXFILE', WS-FL-DD = 'XREFFILE', WS-FL-DD = 'CUSTFILE', WS-FL-DD = 'ACCTFILE'
**Lines:** 298, 300, 302, 306, 309, 761, 779, 797, 815

### BR004: Format customer name and address by concatenating fields with spaces.
**Logic:** STRING first/middle/last names into ST-NAME; addr lines/state/zip into ST-ADD3.
**Conditions:** DELIMITED BY ' ' or SIZE
**Lines:** 462, 472

### BR005: Accumulate and display total transaction amount for matching transactions only.
**Logic:** ADD TRNX-AMT to WS-TOTAL-AMT in inner loop, move to ST-TOTAL-TRAMT.
**Conditions:** TR-JMP <= WS-TRCT (CR-JMP)
**Lines:** 422, 428, 429, 433

### BR006: Limit table to 51 cards x 10 transactions; no explicit overflow check.
**Logic:** OCCURS 51 TIMES / 10 TIMES; counters CR-CNT up to 51, TR-CNT up to 10.
**Conditions:** CR-JMP > CR-CNT, TR-JMP > WS-TRCT (CR-JMP)
**Lines:** 226, 417, 422

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COSTM01](../copybooks/COSTM01.md) | WORKING_STORAGE | CARD-XREF-RECORD for cross-ref fields like XREF-CUST-ID, XREF-ACCT-ID, XREF-CARD-NUM. | 51 |
| [CVACT03Y](../copybooks/CVACT03Y.md) | WORKING_STORAGE | ACCOUNT-RECORD for ACCT-ID, ACCT-CURR-BAL. | 53 |
| [CUSTREC](../copybooks/CUSTREC.md) | WORKING_STORAGE | CUSTOMER-RECORD for names, addresses, FICO score. | 55 |
| [CVACT01Y](../copybooks/CVACT01Y.md) | WORKING_STORAGE | TRNX-RECORD for transaction fields like TRNX-CARD-NUM, TRNX-ID, TRNX-REST, AMT. | 57 |

## Data Flow

### Reads From
- **XREFFILE**: XREF-CUST-ID, XREF-ACCT-ID, XREF-CARD-NUM
  (Lines: 372, 396, 419)
- **CUSTFILE**: CUST-FIRST-NAME, CUST-MIDDLE-NAME, CUST-LAST-NAME, CUST-ADDR-LINE-1, CUST-ADDR-LINE-2, CUST-ADDR-LINE-3, CUST-ADDR-STATE-CD, CUST-ADDR-COUNTRY-CD, CUST-ADDR-ZIP, CUST-FICO-CREDIT-SCORE
  (Lines: 462, 470, 472, 485)
- **ACCTFILE**: ACCT-ID, ACCT-CURR-BAL
  (Lines: 483, 484)
- **TRNXFILE**: TRNX-CARD-NUM, TRNX-ID, TRNX-REST, TRNX-DESC, TRNX-AMT
  (Lines: 757, 827, 829, 676, 677)

### Writes To
- **STMT-FILE**: ST-NAME, ST-ADD1, ST-ADD2, ST-ADD3, ST-ACCT-ID, ST-CURR-BAL, ST-FICO-SCORE, ST-TRANID, ST-TRANDT, ST-TRANAMT, ST-TOTAL-TRAMT
  (Lines: 469, 471, 481, 483, 484, 485, 676, 679, 433)
- **HTML-FILE**: HTML-FIXED-LN (88 levels like HTML-L01 to L80), HTML-L11 (L11-ACCT), L23-NAME, HTML-ADDR-LN, HTML-BSIC-LN, HTML-TRAN-LN
  (Lines: 508, 529, 560, 568, 614, 687)

### Transformations
- **CUST-FIRST-NAME, CUST-MIDDLE-NAME, CUST-LAST-NAME** → **ST-NAME, L23-NAME**: STRING with spaces between names delimited by space/SIZE.
  (Lines: 462, 560)
- **CUST-ADDR-LINE-3, CUST-ADDR-STATE-CD, CUST-ADDR-COUNTRY-CD, CUST-ADDR-ZIP** → **ST-ADD3**: STRING address parts with spaces delimited by space/SIZE.
  (Lines: 472)
- **CUST-ADDR-LINE-1/2** → **ST-ADD1, ST-ADD2, HTML-ADDR-LN**: Direct move for lines 1/2; STRING into HTML-ADDR-LN with <p> tags.
  (Lines: 470, 570, 578)
- **TRNX-AMT (matches)** → **WS-TOTAL-AMT -> ST-TOTAL-TRAMT, ST-TRANAMT**: Sum matching amounts; edited PIC Z(9).99-.
  (Lines: 429, 433, 678)
- **Account/Customer fields** → **HTML-BSIC-LN, HTML-TRAN-LN**: STRING into dynamic lines with <p> labels and </p>.
  (Lines: 614, 687, 699, 711)

## Key Paragraphs

### 0000-START
**Purpose:** Dispatch point using EVALUATE/ALTER/GO TO for sequenced file opens based on WS-FL-DD.
- Calls: 8100-FILE-OPEN
- Lines: 296-315

### 1000-MAINLINE
**Purpose:** Main loop: process each xref record with cust/acct reads, statement creation, trans matching.
- Called by: 8400-ACCTFILE-OPEN
- Calls: 1000-XREFFILE-GET-NEXT, 2000-CUSTFILE-GET, 3000-ACCTFILE-GET, 5000-CREATE-STATEMENT, 4000-TRNXFILE-GET
- Lines: 316-328

### 1000-XREFFILE-GET-NEXT
**Purpose:** Sequential read next xref record via CBSTM03B.
- Called by: 1000-MAINLINE
- Calls: 9999-ABEND-PROGRAM
- Lines: 345-366

### 2000-CUSTFILE-GET
**Purpose:** Key read customer record using XREF-CUST-ID.
- Called by: 1000-MAINLINE
- Calls: 9999-ABEND-PROGRAM
- Lines: 368-389

### 3000-ACCTFILE-GET
**Purpose:** Key read account record using XREF-ACCT-ID.
- Called by: 1000-MAINLINE
- Calls: 9999-ABEND-PROGRAM
- Lines: 392-413

### 4000-TRNXFILE-GET
**Purpose:** Match and output transactions from table for current card, sum total.
- Called by: 1000-MAINLINE
- Calls: 6000-WRITE-TRANS
- Lines: 416-455

### 5000-CREATE-STATEMENT
**Purpose:** Format and write statement headers to both files.
- Called by: 1000-MAINLINE
- Calls: 5100-WRITE-HTML-HEADER, 5200-WRITE-HTML-NMADBS
- Lines: 458-503

### 8100-FILE-OPEN
**Purpose:** Altered dynamic entry for file open sequence.
- Called by: 0000-START
- Calls: 8100-TRNXFILE-OPEN, 8200-XREFFILE-OPEN, 8300-CUSTFILE-OPEN, 8400-ACCTFILE-OPEN
- Lines: 726-729

### 8500-READTRNX-READ
**Purpose:** Loop to sequentially read and index all TRNXFILE records into table.
- Calls: 9999-ABEND-PROGRAM
- Lines: 818-852

### 9999-ABEND-PROGRAM
**Purpose:** Abend on any file error.
- Lines: 921-924

## Error Handling

- **File OPEN RC NOT = '00' or '04':** DISPLAY error, PERFORM 9999-ABEND-PROGRAM
  (Lines: 736, 771, 789, 807)
- **File READ/READ-K RC NOT = '00' (or '10' for seq EOF):** DISPLAY error, PERFORM 9999-ABEND-PROGRAM
  (Lines: 353, 379, 403, 837, 846)
- **File CLOSE RC NOT = '00' or '04':** DISPLAY error, PERFORM 9999-ABEND-PROGRAM
  (Lines: 862, 878, 894, 910)
- **XREFFILE READ RC = '10':** Set END-OF-FILE = 'Y' to exit main loop
  (Lines: 357)
- **Table overflow implied (CR-CNT >51 or TR-CNT >10):** No explicit handling; potential index error
  (Lines: 226, 422)

## Open Questions

- **Exact field layouts in copybooks (e.g., offsets for XREF-CUST-ID)**
  - Context: Copybooks not provided; fields inferred from usage like MOVE XREF-CUST-ID
  - Suggestion: Obtain copybook sources
- **File organizations/access modes confirmed in CBSTM03B?**
  - Context: Inferred VSAM KSDS for key reads, SEQ for others; no explicit SELECT details
  - Suggestion: Analyze CBSTM03B or JCL DD statements
- **Business meaning of XREFFILE structure**
  - Context: Links cards to cust/acct but why separate file?
  - Suggestion: Business documentation or data samples

---
*Generated by War Rig WAR_RIG*