# CBTRN03C

**File:** CBTRN03C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 1
**Analyzed:** 2026-01-18 17:24:01.634993

## Purpose

Batch COBOL program that reads transaction records from TRANSACT-FILE within a date range specified in DATE-PARMS-FILE, performs lookups in XREF-FILE for account details, TRANTYPE-FILE for type descriptions, and TRANCATG-FILE for category descriptions, then generates a detailed report in REPORT-FILE including transaction details, page totals, account totals, and grand totals.

**Business Context:** CardDemo application for printing transaction detail reports
**Program Type:** BATCH
**Citations:** Lines 2, 3, 4, 5, 160, 170, 172, 196, 215

## Inputs

### TRANSACT-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file containing transaction records including card number, type code, category code, amount, process timestamp
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 29, 61, 249

### XREF-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed VSAM file for card cross-reference mapping card number to account ID
- **Copybook:** [CVACT03Y](../copybooks/CVACT03Y.md)
- **Lines:** 33, 67, 485

### TRANTYPE-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed VSAM file for transaction type descriptions keyed by type code
- **Copybook:** [CVTRA03Y](../copybooks/CVTRA03Y.md)
- **Lines:** 39, 72, 495

### TRANCATG-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed VSAM file for transaction category descriptions keyed by type code and category code
- **Copybook:** [CVTRA04Y](../copybooks/CVTRA04Y.md)
- **Lines:** 45, 77, 505

### DATE-PARMS-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file containing start and end dates for report filtering
- **Lines:** 55, 87, 221

## Outputs

### REPORT-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential print file with report headers, transaction details enriched with lookup descriptions, page totals, account totals, and grand totals
- **Copybook:** [CVTRA07Y](../copybooks/CVTRA07Y.md)
- **Lines:** 51, 84, 345

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CEE3ABD](./CEE3ABD.md) | STATIC_CALL | Abend the program with user code 999 | 630 |

## Business Rules

### BR001: Only process transactions where process timestamp is between start date and end date from parameters
**Logic:** Compare TRAN-PROC-TS (1:10) >= WS-START-DATE AND <= WS-END-DATE
**Conditions:** IF TRAN-PROC-TS (1:10) >= WS-START-DATE, AND TRAN-PROC-TS (1:10) <= WS-END-DATE
**Lines:** 173, 174

### BR002: Group transactions by card number and print account totals when card number changes
**Logic:** If WS-CURR-CARD-NUM NOT= TRAN-CARD-NUM and not first time, perform WRITE-ACCOUNT-TOTALS
**Conditions:** IF WS-CURR-CARD-NUM NOT= TRAN-CARD-NUM, IF WS-FIRST-TIME = 'N'
**Lines:** 181, 182

### BR003: Print page totals and new headers every WS-PAGE-SIZE (20) lines
**Logic:** If MOD(WS-LINE-COUNTER, WS-PAGE-SIZE) = 0
**Conditions:** FUNCTION MOD(WS-LINE-COUNTER, WS-PAGE-SIZE) = 0
**Lines:** 282

### BR004: All lookups (XREF, TRANTYPE, TRANCATG) must succeed or abend program
**Logic:** READ with INVALID KEY clause displays error and abends
**Conditions:** INVALID KEY
**Lines:** 486, 496, 506

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVTRA05Y](../copybooks/CVTRA05Y.md) | WORKING_STORAGE | Defines transaction record structure TRAN-RECORD and status TRANFILE-STATUS | 93 |
| [CVACT03Y](../copybooks/CVACT03Y.md) | WORKING_STORAGE | Defines card cross-reference record CARD-XREF-RECORD and status CARDXREF-STATUS | 98 |
| [CVTRA03Y](../copybooks/CVTRA03Y.md) | WORKING_STORAGE | Defines transaction type record TRAN-TYPE-RECORD and status TRANTYPE-STATUS | 103 |
| [CVTRA04Y](../copybooks/CVTRA04Y.md) | WORKING_STORAGE | Defines transaction category record TRAN-CAT-RECORD and status TRANCATG-STATUS | 108 |
| [CVTRA07Y](../copybooks/CVTRA07Y.md) | WORKING_STORAGE | Defines report record structures like TRANSACTION-DETAIL-REPORT, headers, totals, and status TRANREPT-STATUS | 113 |

## Data Flow

### Reads From
- **TRANSACT-FILE**: TRAN-CARD-NUM, TRAN-TYPE-CD, TRAN-CAT-CD, TRAN-AMT, TRAN-PROC-TS, TRAN-ID, TRAN-SOURCE
  (Lines: 172, 180, 186, 189, 193, 200, 363, 370)
- **XREF-FILE**: XREF-ACCT-ID
  (Lines: 187, 364)
- **TRANTYPE-FILE**: TRAN-TYPE-DESC
  (Lines: 190, 365, 366)
- **TRANCATG-FILE**: TRAN-CAT-TYPE-DESC
  (Lines: 195, 367, 368)
- **DATE-PARMS-FILE**: WS-START-DATE, WS-END-DATE
  (Lines: 168, 221, 173)

### Writes To
- **REPORT-FILE**: TRANSACTION-DETAIL-REPORT, REPORT-NAME-HEADER, TRANSACTION-HEADER-1, TRANSACTION-HEADER-2, REPORT-PAGE-TOTALS, REPORT-ACCOUNT-TOTALS, REPORT-GRAND-TOTALS
  (Lines: 372, 326, 333, 337, 296, 308, 320)

### Transformations
- **TRAN-AMT** → **WS-PAGE-TOTAL**: Add to page total accumulator
  (Lines: 287)
- **TRAN-AMT** → **WS-ACCOUNT-TOTAL**: Add to account total accumulator
  (Lines: 287)
- **TRAN-AMT** → **WS-GRAND-TOTAL**: Add page total to grand total after page totals written
  (Lines: 297)
- **TRAN-CARD-NUM** → **FD-XREF-CARD-NUM**: Copy for XREF lookup key
  (Lines: 186)
- **TRAN-TYPE-CD** → **FD-TRAN-TYPE**: Copy for TRANTYPE lookup key
  (Lines: 189)
- **TRAN-TYPE-CD** → **FD-TRAN-TYPE-CD OF FD-TRAN-CAT-KEY**: Copy for TRANCATG lookup composite key
  (Lines: 191)
- **TRAN-CAT-CD** → **FD-TRAN-CAT-CD OF FD-TRAN-CAT-KEY**: Copy for TRANCATG lookup composite key
  (Lines: 194)

## Key Paragraphs

### 0000-TRANFILE-OPEN
**Purpose:** Open input TRANSACT-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 376-392

### 0100-REPTFILE-OPEN
**Purpose:** Open output REPORT-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 394-410

### 0200-CARDXREF-OPEN
**Purpose:** Open input XREF-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 412-428

### 0300-TRANTYPE-OPEN
**Purpose:** Open input TRANTYPE-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 430-446

### 0400-TRANCATG-OPEN
**Purpose:** Open input TRANCATG-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 448-464

### 0500-DATEPARM-OPEN
**Purpose:** Open input DATE-PARMS-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 466-482

### 0550-DATEPARM-READ
**Purpose:** Read date parameters and set EOF if end of file
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 220-244

### 1000-TRANFILE-GET-NEXT
**Purpose:** Sequential read next transaction record and set EOF if end
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 248-272

### 1100-WRITE-TRANSACTION-REPORT
**Purpose:** Main report writing logic: headers on first time or page end, accumulate totals, write detail
- Calls: 1120-WRITE-HEADERS, 1110-WRITE-PAGE-TOTALS, 1120-WRITE-DETAIL
- Lines: 274-290

### 1110-WRITE-PAGE-TOTALS
**Purpose:** Write page total line, reset page total, add to grand, write blank header line
- Calls: 1111-WRITE-REPORT-REC
- Lines: 293-304

### 1120-WRITE-ACCOUNT-TOTALS
**Purpose:** Write account total line, reset account total, write blank header line
- Calls: 1111-WRITE-REPORT-REC
- Lines: 306-316

### 1110-WRITE-GRAND-TOTALS
**Purpose:** Write grand total line
- Calls: 1111-WRITE-REPORT-REC
- Lines: 318-322

### 1120-WRITE-HEADERS
**Purpose:** Write report name header, blank line, transaction headers 1 and 2
- Calls: 1111-WRITE-REPORT-REC
- Lines: 324-341

### 1111-WRITE-REPORT-REC
**Purpose:** Write report record and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 343-359

### 1120-WRITE-DETAIL
**Purpose:** Populate and write transaction detail report line with looked-up descriptions
- Calls: 1111-WRITE-REPORT-REC
- Lines: 361-374

### 1500-A-LOOKUP-XREF
**Purpose:** Random read XREF-FILE by card number, abend on invalid key
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 484-492

### 1500-B-LOOKUP-TRANTYPE
**Purpose:** Random read TRANTYPE-FILE by type code, abend on invalid key
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 494-502

### 1500-C-LOOKUP-TRANCATG
**Purpose:** Random read TRANCATG-FILE by type and cat code, abend on invalid key
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 504-512

### 9000-TRANFILE-CLOSE
**Purpose:** Close TRANSACT-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 514-530

### 9100-REPTFILE-CLOSE
**Purpose:** Close REPORT-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 532-548

### 9200-CARDXREF-CLOSE
**Purpose:** Close XREF-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 551-567

### 9300-TRANTYPE-CLOSE
**Purpose:** Close TRANTYPE-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 569-585

### 9400-TRANCATG-CLOSE
**Purpose:** Close TRANCATG-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 587-603

### 9500-DATEPARM-CLOSE
**Purpose:** Close DATE-PARMS-FILE and validate status
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 605-621

### 9999-ABEND-PROGRAM
**Purpose:** Call CEE3ABD to abend program with code 999
- Lines: 626-632

### 9910-DISPLAY-IO-STATUS
**Purpose:** Display file IO status in formatted numeric form
- Lines: 633-646

## Error Handling

- **File status not '00' on open/read/write/close:** Display error message, perform DISPLAY-IO-STATUS, then ABEND-PROGRAM
  (Lines: 268, 356, 389, 407, 425, 443, 461, 479, 489, 499, 509, 527, 545, 564, 582, 600, 618)
- **INVALID KEY on VSAM READs:** Display invalid key message, set IO-STATUS to 23, perform DISPLAY-IO-STATUS, ABEND-PROGRAM
  (Lines: 486, 496, 506)

## Open Questions

- **Exact field layouts in copybooks CVTRA05Y, CVACT03Y, CVTRA03Y, CVTRA04Y, CVTRA07Y**
  - Context: Copybooks not expanded in source; inferred from usage but precise PIC clauses unknown
  - Suggestion: Provide copybook source files

---
*Generated by War Rig WAR_RIG*