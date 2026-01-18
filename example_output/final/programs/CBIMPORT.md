# CBIMPORT

**File:** CBIMPORT.cbl
**Type:** COBOL
**Status:** FinalStatus.VALHALLA
**Iterations:** 2
**Analyzed:** 2026-01-18 18:06:09.292860

## Purpose

CBIMPORT reads an indexed sequential export file containing mixed customer, account, xref, transaction, and card records distinguished by record type. It maps fields to normalized sequential output files per type, logs unknown records to error file, tracks statistics, and abends on file I/O failures. Performs placeholder validation and displays import statistics upon completion.

**Business Context:** Branch Migration Import: imports customer profiles from multi-record export, normalizes to separate files, intended for data integrity validation via checksums (placeholder), generates statistics and error reports.
**Program Type:** BATCH
**Citations:** Lines 5, 8, 9, 10, 11, 28, 29, 30, 31, 32, 165

## Inputs

### EXPORT-INPUT
- **Type:** FILE_SEQUENTIAL
- **Description:** Indexed sequential file (ORGANIZATION INDEXED, ACCESS SEQUENTIAL) with 500-character records holding mixed types identified by EXPORT-REC-TYPE (C=Customer, A=Account, X=XREF, T=Transaction, D=Card).
- **Copybook:** [CVEXPORT](../copybooks/CVEXPORT.md)
- **Lines:** 37, 38, 39, 40, 76, 77, 78, 79, 113, 261

## Outputs

### CUSTOMER-OUTPUT
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file receiving normalized 500-character customer records after field mapping from export.
- **Copybook:** [CVCUS01Y](../copybooks/CVCUS01Y.md)
- **Lines:** 43, 44, 45, 46, 81, 82, 83, 84, 312

### ACCOUNT-OUTPUT
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file receiving normalized 300-character account records after field mapping from export.
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 48, 49, 50, 51, 86, 87, 88, 89, 341

### XREF-OUTPUT
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file receiving normalized 50-character card xref records after field mapping from export.
- **Copybook:** [CVACT03Y](../copybooks/CVACT03Y.md)
- **Lines:** 53, 54, 55, 56, 91, 92, 93, 94, 361

### TRANSACTION-OUTPUT
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file receiving normalized 350-character transaction records after field mapping from export.
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 58, 59, 60, 61, 96, 97, 98, 99, 391

### CARD-OUTPUT
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file receiving normalized 150-character card records after field mapping from export.
- **Copybook:** [CVACT02Y](../copybooks/CVACT02Y.md)
- **Lines:** 63, 64, 65, 66, 101, 102, 103, 104, 414

### ERROR-OUTPUT
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file receiving 132-character error records for unknown record types, including timestamp, type, sequence, and message.
- **Lines:** 68, 69, 70, 71, 106, 107, 108, 109, 439

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CEE3ABD](./CEE3ABD.md) | STATIC_CALL | Invoked to abend program on critical file I/O errors. | 484 |

## Business Rules

### BR001: Route records by EXPORT-REC-TYPE to specific processing: 'C'=Customer, 'A'=Account, 'X'=XREF, 'T'=Transaction, 'D'=Card, OTHER=Unknown.
**Logic:** EVALUATE on EXPORT-REC-TYPE with PERFORM to typed paragraphs.
**Conditions:** WHEN 'C', WHEN 'A', WHEN 'X', WHEN 'T', WHEN 'D', WHEN OTHER
**Lines:** 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284

### BR002: Abend on file open/read/write failures (status not '00', excluding EOF on read).
**Logic:** Post-OPEN/READ/WRITE checks via 88-level conditions trigger DISPLAY error and PERFORM 9999-ABEND-PROGRAM.
**Conditions:** IF NOT WS-*-OK, IF NOT WS-EXPORT-OK AND NOT WS-EXPORT-EOF
**Lines:** 199, 206, 213, 220, 227, 234, 241, 263, 314, 343, 363, 393, 416

### BR003: Log unknown record types to ERROR-OUTPUT with current date timestamp, record type, sequence number, and fixed message.
**Logic:** Increment unknown count, populate/move to WS-ERROR-RECORD, perform WRITE.
**Conditions:** WHEN OTHER
**Lines:** 427, 429, 430, 431, 432, 434, 439

### BR004: Accumulate statistics counters for total reads and records imported per type (customer, account, xref, transaction, card, errors, unknowns).
**Logic:** ADD 1 TO WS-TOTAL-RECORDS-READ on each loop iteration; ADD 1 TO type-specific counters after successful WRITE.
**Conditions:** ADD 1 TO WS-TOTAL-RECORDS-READ, ADD 1 TO WS-*-RECORDS-IMPORTED
**Lines:** 252, 320, 349, 369, 399, 422, 427, 446

### BR005: Placeholder import validation: displays fixed completion messages without checks.
**Logic:** DISPLAY messages indicating completion and no errors detected (no actual logic).
**Lines:** 451, 452

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVCUS01Y](../copybooks/CVCUS01Y.md) | FILE_SECTION | Defines CUSTOMER-RECORD structure for customer output file. | 84 |
| [CVACT01Y](../copybooks/CVACT01Y.md) | FILE_SECTION | Defines ACCOUNT-RECORD structure for account output file. | 89 |
| [CVACT03Y](../copybooks/CVACT03Y.md) | FILE_SECTION | Defines CARD-XREF-RECORD structure for xref output file. | 94 |
| [CVTRA05Y](../copybooks/CVTRA05Y.md) | FILE_SECTION | Defines TRAN-RECORD structure for transaction output file. | 99 |
| [CVACT02Y](../copybooks/CVACT02Y.md) | FILE_SECTION | Defines CARD-RECORD structure for card output file. | 104 |
| [CVEXPORT](../copybooks/CVEXPORT.md) | WORKING_STORAGE | Defines EXPORT-RECORD structure mapped from EXPORT-INPUT-RECORD for input processing. | 113 |

## Data Flow

### Reads From
- **EXPORT-INPUT**: EXPORT-REC-TYPE, EXPORT-SEQUENCE-NUM, EXP-CUST-ID, EXP-CUST-FIRST-NAME, EXP-CUST-MIDDLE-NAME, EXP-CUST-LAST-NAME, EXP-CUST-ADDR-LINE, EXP-CUST-ADDR-STATE-CD, EXP-CUST-ADDR-COUNTRY-CD, EXP-CUST-ADDR-ZIP, EXP-CUST-PHONE-NUM, EXP-CUST-SSN, EXP-CUST-GOVT-ISSUED-ID, EXP-CUST-DOB-YYYY-MM-DD, EXP-CUST-EFT-ACCOUNT-ID, EXP-CUST-PRI-CARD-HOLDER-IND, EXP-CUST-FICO-CREDIT-SCORE, EXP-ACCT-ID, EXP-ACCT-ACTIVE-STATUS, EXP-ACCT-CURR-BAL, EXP-ACCT-CREDIT-LIMIT, EXP-ACCT-CASH-CREDIT-LIMIT, EXP-ACCT-OPEN-DATE, EXP-ACCT-EXPIRAION-DATE, EXP-ACCT-REISSUE-DATE, EXP-ACCT-CURR-CYC-CREDIT, EXP-ACCT-CURR-CYC-DEBIT, EXP-ACCT-ADDR-ZIP, EXP-ACCT-GROUP-ID, EXP-XREF-CARD-NUM, EXP-XREF-CUST-ID, EXP-XREF-ACCT-ID, EXP-TRAN-ID, EXP-TRAN-TYPE-CD, EXP-TRAN-CAT-CD, EXP-TRAN-SOURCE, EXP-TRAN-DESC, EXP-TRAN-AMT, EXP-TRAN-MERCHANT-ID, EXP-TRAN-MERCHANT-NAME, EXP-TRAN-MERCHANT-CITY, EXP-TRAN-MERCHANT-ZIP, EXP-TRAN-CARD-NUM, EXP-TRAN-ORIG-TS, EXP-TRAN-PROC-TS, EXP-CARD-NUM, EXP-CARD-ACCT-ID, EXP-CARD-CVV-CD, EXP-CARD-EMBOSSED-NAME, EXP-CARD-EXPIRAION-DATE, EXP-CARD-ACTIVE-STATUS
  (Lines: 261, 272, 293, 328, 357, 377, 407)

### Writes To
- **CUSTOMER-OUTPUT**: CUST-ID, CUST-FIRST-NAME, CUST-MIDDLE-NAME, CUST-LAST-NAME, CUST-ADDR-LINE-1, CUST-ADDR-LINE-2, CUST-ADDR-LINE-3, CUST-ADDR-STATE-CD, CUST-ADDR-COUNTRY-CD, CUST-ADDR-ZIP, CUST-PHONE-NUM-1, CUST-PHONE-NUM-2, CUST-SSN, CUST-GOVT-ISSUED-ID, CUST-DOB-YYYY-MM-DD, CUST-EFT-ACCOUNT-ID, CUST-PRI-CARD-HOLDER-IND, CUST-FICO-CREDIT-SCORE
  (Lines: 312)
- **ACCOUNT-OUTPUT**: ACCT-ID, ACCT-ACTIVE-STATUS, ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, ACCT-OPEN-DATE, ACCT-EXPIRAION-DATE, ACCT-REISSUE-DATE, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT, ACCT-ADDR-ZIP, ACCT-GROUP-ID
  (Lines: 341)
- **XREF-OUTPUT**: XREF-CARD-NUM, XREF-CUST-ID, XREF-ACCT-ID
  (Lines: 361)
- **TRANSACTION-OUTPUT**: TRAN-ID, TRAN-TYPE-CD, TRAN-CAT-CD, TRAN-SOURCE, TRAN-DESC, TRAN-AMT, TRAN-MERCHANT-ID, TRAN-MERCHANT-NAME, TRAN-MERCHANT-CITY, TRAN-MERCHANT-ZIP, TRAN-CARD-NUM, TRAN-ORIG-TS, TRAN-PROC-TS
  (Lines: 391)
- **CARD-OUTPUT**: CARD-NUM, CARD-ACCT-ID, CARD-CVV-CD, CARD-EMBOSSED-NAME, CARD-EXPIRAION-DATE, CARD-ACTIVE-STATUS
  (Lines: 414)
- **ERROR-OUTPUT**: ERR-TIMESTAMP, ERR-RECORD-TYPE, ERR-SEQUENCE, ERR-MESSAGE
  (Lines: 439)

### Transformations
- **EXP-CUST-ID** → **CUST-ID**: Direct MOVE after INITIALIZE CUSTOMER-RECORD.
  (Lines: 290, 293)
- **EXP-ACCT-ID** → **ACCT-ID**: Direct MOVE after INITIALIZE ACCOUNT-RECORD.
  (Lines: 325, 328)
- **EXP-XREF-CARD-NUM** → **XREF-CARD-NUM**: Direct MOVE after INITIALIZE CARD-XREF-RECORD.
  (Lines: 354, 357)
- **EXP-TRAN-ID** → **TRAN-ID**: Direct MOVE after INITIALIZE TRAN-RECORD.
  (Lines: 374, 377)
- **EXP-CARD-NUM** → **CARD-NUM**: Direct MOVE after INITIALIZE CARD-RECORD.
  (Lines: 404, 407)
- **EXPORT-REC-TYPE** → **ERR-RECORD-TYPE**: Direct MOVE to error record for unknown types; ERR-TIMESTAMP from FUNCTION CURRENT-DATE.
  (Lines: 429, 430)

## Key Paragraphs

### 0000-MAIN-PROCESSING
**Purpose:** Orchestrates overall flow: initialize, process export, validate, finalize.
- Calls: 1000-INITIALIZE, 2000-PROCESS-EXPORT-FILE, 3000-VALIDATE-IMPORT, 4000-FINALIZE
- Lines: 165-173

### 1000-INITIALIZE
**Purpose:** Formats and sets WS-IMPORT-DATE/TIME from CURRENT-DATE, displays start message, opens files.
- Called by: 0000-MAIN-PROCESSING
- Calls: 1100-OPEN-FILES
- Lines: 174-195

### 1100-OPEN-FILES
**Purpose:** Opens all files (INPUT EXPORT-INPUT, OUTPUT others), abends on any failure.
- Called by: 1000-INITIALIZE
- Calls: 9999-ABEND-PROGRAM
- Lines: 196-245

### 2000-PROCESS-EXPORT-FILE
**Purpose:** Main processing loop: read until EOF, increment total read count, process by type.
- Called by: 0000-MAIN-PROCESSING
- Calls: 2100-READ-EXPORT-RECORD, 2200-PROCESS-RECORD-BY-TYPE
- Lines: 248-256

### 2100-READ-EXPORT-RECORD
**Purpose:** READs EXPORT-INPUT INTO EXPORT-RECORD, abends on non-EOF failure.
- Called by: 2000-PROCESS-EXPORT-FILE
- Calls: 9999-ABEND-PROGRAM
- Lines: 259-267

### 2200-PROCESS-RECORD-BY-TYPE
**Purpose:** EVALUATEs EXPORT-REC-TYPE to dispatch to type-specific processing.
- Called by: 2000-PROCESS-EXPORT-FILE
- Calls: 2300-PROCESS-CUSTOMER-RECORD, 2400-PROCESS-ACCOUNT-RECORD, 2500-PROCESS-XREF-RECORD, 2600-PROCESS-TRAN-RECORD, 2650-PROCESS-CARD-RECORD, 2700-PROCESS-UNKNOWN-RECORD
- Lines: 270-285

### 2300-PROCESS-CUSTOMER-RECORD
**Purpose:** INITIALIZEs and MOVE fields from EXP-CUST-* to CUSTOMER-RECORD, WRITEs, increments counter, abends on failure.
- Called by: 2200-PROCESS-RECORD-BY-TYPE
- Calls: 9999-ABEND-PROGRAM
- Lines: 288-322

### 2400-PROCESS-ACCOUNT-RECORD
**Purpose:** INITIALIZEs and MOVE fields from EXP-ACCT-* to ACCOUNT-RECORD, WRITEs, increments counter, abends on failure.
- Called by: 2200-PROCESS-RECORD-BY-TYPE
- Calls: 9999-ABEND-PROGRAM
- Lines: 323-351

### 2500-PROCESS-XREF-RECORD
**Purpose:** INITIALIZEs and MOVE EXP-XREF-* to CARD-XREF-RECORD, WRITEs, increments counter, abends on failure.
- Called by: 2200-PROCESS-RECORD-BY-TYPE
- Calls: 9999-ABEND-PROGRAM
- Lines: 352-371

### 2600-PROCESS-TRAN-RECORD
**Purpose:** INITIALIZEs and MOVE EXP-TRAN-* to TRAN-RECORD, WRITEs, increments counter, abends on failure.
- Called by: 2200-PROCESS-RECORD-BY-TYPE
- Calls: 9999-ABEND-PROGRAM
- Lines: 372-401

### 2650-PROCESS-CARD-RECORD
**Purpose:** INITIALIZEs and MOVE EXP-CARD-* to CARD-RECORD, WRITEs, increments counter, abends on failure.
- Called by: 2200-PROCESS-RECORD-BY-TYPE
- Calls: 9999-ABEND-PROGRAM
- Lines: 402-424

### 2700-PROCESS-UNKNOWN-RECORD
**Purpose:** Increments unknown count, populates error record, performs write error.
- Called by: 2200-PROCESS-RECORD-BY-TYPE
- Calls: 2750-WRITE-ERROR
- Lines: 425-436

### 2750-WRITE-ERROR
**Purpose:** WRITEs ERROR-OUTPUT-RECORD FROM WS-ERROR-RECORD, increments error count, displays status if fail but continues.
- Called by: 2700-PROCESS-UNKNOWN-RECORD
- Lines: 437-448

### 3000-VALIDATE-IMPORT
**Purpose:** Placeholder: displays import validation completion messages.
- Called by: 0000-MAIN-PROCESSING
- Lines: 449-454

### 4000-FINALIZE
**Purpose:** CLOSEs all files, displays all import statistics.
- Called by: 0000-MAIN-PROCESSING
- Lines: 455-478

### 9999-ABEND-PROGRAM
**Purpose:** Displays abend message and CALLs CEE3ABD.
- Called by: 1100-OPEN-FILES, 2100-READ-EXPORT-RECORD, 2300-PROCESS-CUSTOMER-RECORD, 2400-PROCESS-ACCOUNT-RECORD, 2500-PROCESS-XREF-RECORD, 2600-PROCESS-TRAN-RECORD, 2650-PROCESS-CARD-RECORD
- Lines: 481-484

## Error Handling

- **OPEN fails for any file (NOT WS-*-OK):** DISPLAY file-specific error message with status, PERFORM 9999-ABEND-PROGRAM
  (Lines: 199, 206, 213, 220, 227, 234, 241)
- **READ EXPORT-INPUT fails (NOT WS-EXPORT-OK AND NOT WS-EXPORT-EOF):** DISPLAY error message with status, PERFORM 9999-ABEND-PROGRAM
  (Lines: 263)
- **WRITE fails for CUSTOMER/ACCOUNT/XREF/TRANSACTION/CARD-OUTPUT (NOT WS-*-OK):** DISPLAY file-specific error message with status, PERFORM 9999-ABEND-PROGRAM
  (Lines: 314, 343, 363, 393, 416)
- **WRITE ERROR-OUTPUT fails (NOT WS-ERROR-OK):** DISPLAY error message with status, but continue processing (no abend)
  (Lines: 441)

## Open Questions

- **Precise field definitions and layouts in copybooks (CVCUS01Y, CVACT01Y, etc., CVEXPORT)**
  - Context: Referenced but source contents unavailable; fields inferred solely from MOVE statements.
  - Suggestion: Obtain and analyze copybook source files.

---
*Generated by War Rig WAR_RIG*