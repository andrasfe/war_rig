# CBTRN01C

**File:** CBTRN01C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 2
**Analyzed:** 2026-01-18 16:40:10.736539

## Purpose

Batch COBOL program that validates daily transaction records by sequentially reading DALYTRAN-FILE, looking up card number in indexed XREF-FILE to obtain account ID, and validating account existence via keyed read of ACCOUNT-FILE, displaying success/error messages but performing no updates or writes despite header comment claiming to 'Post the records'. Files CUSTOMER-FILE, CARD-FILE, and TRANSACT-FILE are opened as INPUT and closed without any I/O operations.

**Business Context:** CardDemo application serving transaction posting validation (actual posting absent)
**Program Type:** BATCH
**Citations:** Lines 2, 3, 4, 5, 29, 40, 52, 164, 172, 176

## Inputs

### DALYTRAN-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential daily transaction records with transaction ID (FD-TRAN-ID PIC X(16)) and customer data (FD-CUST-DATA PIC X(334)) including card number (DALYTRAN-CARD-NUM)
- **Copybook:** [CVTRA06Y](../copybooks/CVTRA06Y.md)
- **Lines:** 29, 30, 31, 67, 68, 203, 170

### XREF-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed cross-reference file keyed by card number (FD-XREF-CARD-NUM PIC X(16)) containing account ID (XREF-ACCT-ID) and customer ID (XREF-CUST-ID)
- **Copybook:** [CVACT03Y](../copybooks/CVACT03Y.md)
- **Lines:** 40, 41, 42, 43, 78, 229

### ACCOUNT-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed account master file keyed by account ID (FD-ACCT-ID PIC 9(11)) containing account data (FD-ACCT-DATA PIC X(289))
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 52, 53, 54, 55, 88, 243

### CUSTOMER-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed customer file keyed by customer ID (FD-CUST-ID PIC 9(09)) opened but never read
- **Copybook:** [CVCUS01Y](../copybooks/CVCUS01Y.md)
- **Lines:** 34, 35, 36, 37, 72

### CARD-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed card file keyed by card number (FD-CARD-NUM PIC X(16)) opened but never read
- **Copybook:** [CVACT02Y](../copybooks/CVACT02Y.md)
- **Lines:** 46, 47, 48, 49, 83

### TRANSACT-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed transaction output file keyed by transaction ID (FD-TRANS-ID PIC X(16)) opened as INPUT but never read or written
- **Copybook:** [CVTRA05Y](../copybooks/CVTRA05Y.md)
- **Lines:** 58, 59, 60, 61, 93

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CEE3ABD](./CEE3ABD.md) | STATIC_CALL | Abnormal program termination (abend) with user completion code 999 | 473 |

## Business Rules

### BR001: Skip transaction processing if card number not found in XREF-FILE (INVALID KEY)
**Logic:** Display 'INVALID CARD NUMBER FOR XREF', set WS-XREF-READ-STATUS to 4, skip account read
**Conditions:** INVALID KEY on READ XREF-FILE
**Lines:** 231, 232, 233

### BR002: Display account not found if XREF valid but account missing (INVALID KEY on ACCOUNT-FILE)
**Logic:** After successful XREF, perform account read; on INVALID KEY display message and set WS-ACCT-READ-STATUS to 4 but continue processing next transaction
**Conditions:** WS-XREF-READ-STATUS = 0, INVALID KEY on READ ACCOUNT-FILE
**Lines:** 173, 245, 246, 247

### BR003: Terminate main loop on EOF of DALYTRAN-FILE
**Logic:** On READ status '10', set END-OF-DAILY-TRANS-FILE to 'Y'
**Conditions:** DALYTRAN-STATUS = '10'
**Lines:** 207, 217

### BR004: On successful XREF lookup, display validation details and proceed to account validation
**Logic:** Display 'SUCCESSFUL READ OF XREF', card number, account ID, customer ID
**Conditions:** NOT INVALID KEY on READ XREF-FILE
**Lines:** 235, 236, 237, 238

### BR005: On successful ACCOUNT-FILE read, display success message
**Logic:** Display 'SUCCESSFUL READ OF ACCOUNT FILE'
**Conditions:** NOT INVALID KEY on READ ACCOUNT-FILE
**Lines:** 249

### BR006: Abend on non-zero/non-EOF file status during DALYTRAN-FILE READ
**Logic:** Display error, show IO status, abend
**Conditions:** DALYTRAN-STATUS NOT = '00' or '10'
**Lines:** 219, 221, 222

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVTRA06Y](../copybooks/CVTRA06Y.md) | WORKING_STORAGE | Defines DALYTRAN-RECORD structure populated on READ from DALYTRAN-FILE (includes DALYTRAN-CARD-NUM, DALYTRAN-ID) | 99 |
| [CVCUS01Y](../copybooks/CVCUS01Y.md) | WORKING_STORAGE | Defines customer record structure for CUSTOMER-FILE (FD-CUSTFILE-REC); file opened but copybook fields unused | 104 |
| [CVACT03Y](../copybooks/CVACT03Y.md) | WORKING_STORAGE | Defines CARD-XREF-RECORD structure (XREF-CARD-NUM, XREF-ACCT-ID, XREF-CUST-ID) populated on READ from XREF-FILE | 109 |
| [CVACT02Y](../copybooks/CVACT02Y.md) | WORKING_STORAGE | Defines card record structure for CARD-FILE (FD-CARDFILE-REC); file opened but copybook fields unused | 114 |
| [CVACT01Y](../copybooks/CVACT01Y.md) | WORKING_STORAGE | Defines ACCOUNT-RECORD structure populated on READ from ACCOUNT-FILE | 119 |
| [CVTRA05Y](../copybooks/CVTRA05Y.md) | WORKING_STORAGE | Defines output transaction record structure for TRANSACT-FILE (FD-TRANFILE-REC); file opened INPUT but never used | 124 |

## Data Flow

### Reads From
- **DALYTRAN-FILE**: DALYTRAN-CARD-NUM, DALYTRAN-ID
  (Lines: 170, 183)
- **XREF-FILE**: XREF-ACCT-ID, XREF-CUST-ID
  (Lines: 237)
- **ACCOUNT-FILE**: all fields
  (Lines: 243)

### Transformations
- **DALYTRAN-CARD-NUM** → **XREF-CARD-NUM**: Move card number from daily transaction record to XREF working storage key field
  (Lines: 171)
- **XREF-CARD-NUM** → **FD-XREF-CARD-NUM**: Move to file description key for XREF-FILE keyed read
  (Lines: 228)
- **XREF-ACCT-ID** → **ACCT-ID**: Move account ID from XREF record to ACCOUNT working storage key
  (Lines: 175)
- **ACCT-ID** → **FD-ACCT-ID**: Move to file description key for ACCOUNT-FILE keyed read
  (Lines: 242)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Orchestrates file opens, main processing loop (read daily trans, lookup xref, read account, validate), file closes
- Calls: 0000-DALYTRAN-OPEN, 0100-CUSTFILE-OPEN, 0200-XREFFILE-OPEN, 0300-CARDFILE-OPEN, 0400-ACCTFILE-OPEN, 0500-TRANFILE-OPEN, 1000-DALYTRAN-GET-NEXT, 2000-LOOKUP-XREF, 3000-READ-ACCOUNT, 9000-DALYTRAN-CLOSE, 9100-CUSTFILE-CLOSE, 9200-XREFFILE-CLOSE, 9300-CARDFILE-CLOSE, 9400-ACCTFILE-CLOSE, 9500-TRANFILE-CLOSE
- Lines: 155-197

### 1000-DALYTRAN-GET-NEXT
**Purpose:** Sequential READ DALYTRAN-FILE, set EOF flag on '10', abend on other errors
- Called by: MAIN-PARA
- Calls: Z-DISPLAY-IO-STATUS, Z-ABEND-PROGRAM
- Lines: 202-225

### 2000-LOOKUP-XREF
**Purpose:** Keyed READ XREF-FILE by card number, handle INVALID KEY vs success with displays
- Called by: MAIN-PARA
- Lines: 227-240

### 3000-READ-ACCOUNT
**Purpose:** Keyed READ ACCOUNT-FILE by account ID, handle INVALID KEY vs success with displays
- Called by: MAIN-PARA
- Lines: 241-251

### 0000-DALYTRAN-OPEN
**Purpose:** OPEN INPUT DALYTRAN-FILE, abend on error
- Called by: MAIN-PARA
- Calls: Z-DISPLAY-IO-STATUS, Z-ABEND-PROGRAM
- Lines: 252-268

### Z-ABEND-PROGRAM
**Purpose:** CALL CEE3ABD to abend program
- Called by: 0000-DALYTRAN-OPEN, 0100-CUSTFILE-OPEN, 0200-XREFFILE-OPEN, 0300-CARDFILE-OPEN, 0400-ACCTFILE-OPEN, 0500-TRANFILE-OPEN, 9000-DALYTRAN-CLOSE, 9100-CUSTFILE-CLOSE, 9200-XREFFILE-CLOSE, 9300-CARDFILE-CLOSE, 9400-ACCTFILE-CLOSE, 9500-TRANFILE-CLOSE, 1000-DALYTRAN-GET-NEXT
- Lines: 469-475

### Z-DISPLAY-IO-STATUS
**Purpose:** Format and DISPLAY file status code numerically
- Called by: 0000-DALYTRAN-OPEN, 0100-CUSTFILE-OPEN, 0200-XREFFILE-OPEN, 0300-CARDFILE-OPEN, 0400-ACCTFILE-OPEN, 0500-TRANFILE-OPEN, 9000-DALYTRAN-CLOSE, 9100-CUSTFILE-CLOSE, 9200-XREFFILE-CLOSE, 9300-CARDFILE-CLOSE, 9400-ACCTFILE-CLOSE, 9500-TRANFILE-CLOSE, 1000-DALYTRAN-GET-NEXT
- Lines: 476-489

## Error Handling

- **DALYTRAN-STATUS NOT = '00' and NOT = '10' on READ:** DISPLAY error message, PERFORM Z-DISPLAY-IO-STATUS, PERFORM Z-ABEND-PROGRAM
  (Lines: 219, 221, 222)
- **File STATUS NOT = '00' on any OPEN INPUT:** DISPLAY file-specific error, PERFORM Z-DISPLAY-IO-STATUS, PERFORM Z-ABEND-PROGRAM
  (Lines: 258, 263, 265, 277, 282, 284, 295, 300, 302, 313, 318, 320, 331, 336, 338, 349, 354, 356)
- **File STATUS NOT = '00' on any CLOSE:** DISPLAY file-specific error, PERFORM Z-DISPLAY-IO-STATUS, PERFORM Z-ABEND-PROGRAM
  (Lines: 367, 372, 374, 385, 390, 392, 403, 408, 410, 421, 426, 428, 439, 444, 446, 457, 462, 464)
- **INVALID KEY on READ XREF-FILE:** DISPLAY 'INVALID CARD NUMBER FOR XREF', MOVE 4 TO WS-XREF-READ-STATUS, skip account read
  (Lines: 232, 233)
- **INVALID KEY on READ ACCOUNT-FILE:** DISPLAY 'INVALID ACCOUNT NUMBER FOUND', MOVE 4 TO WS-ACCT-READ-STATUS, continue to next transaction
  (Lines: 246, 247)

## Open Questions

- **Full contents and field layouts of copybooks CVTRA06Y, CVCUS01Y, CVACT03Y, CVACT02Y, CVACT01Y, CVTRA05Y**
  - Context: Copybooks copied into WORKING-STORAGE but source unavailable; fields like DALYTRAN-CARD-NUM inferred from usage
  - Suggestion: Provide copybook source code
- **Purpose of opening CUSTOMER-FILE, CARD-FILE, TRANSACT-FILE as INPUT without any READ/WRITE/REWRITE**
  - Context: Opened and closed in dedicated paragraphs (271-287, 307-323, 343-359) with error handling, but no I/O in main logic or elsewhere
  - Suggestion: Possibly code incompleteness, future enhancement, or dependency on copybook logic; review JCL/PROC or runtime behavior
- **Why no transaction posting despite header comment 'Post the records from daily transaction file'**
  - Context: Program only validates via lookups/displays, no writes to TRANSACT-FILE or updates to ACCOUNT-FILE/CUSTOMER-FILE
  - Suggestion: Likely incomplete implementation; check for missing WRITE/REWRITE logic or external posting mechanism

---
*Generated by War Rig WAR_RIG*