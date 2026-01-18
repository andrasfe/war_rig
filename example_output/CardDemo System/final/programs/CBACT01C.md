# CBACT01C

**File:** CBACT01C.cbl
**Type:** COBOL
**Status:** FinalStatus.VALHALLA
**Iterations:** 2
**Analyzed:** 2026-01-18 17:46:49.089641

## Purpose

Batch COBOL program reads sequential records from indexed VSAM ACCTFILE-FILE containing account data (from CVACT01Y), displays each record fields, transforms data by formatting reissue date via COBDATFT call (type '2'), overriding zero ACCT-CURR-CYC-DEBIT to 2525.00, populating ARRY-FILE with hardcoded balance/debit values for some array elements, generating two variable-length records per account for VBRC-FILE (12-byte ID+status and 39-byte ID+bal+limit+YYYY), and writing to three sequential output files until EOF on input.

**Business Context:** CardDemo application account file processing, transformation, and export to multiple formats
**Program Type:** BATCH
**Citations:** Lines 2, 3, 4, 5, 141, 147, 166, 236, 231, 255, 288, 303

## Inputs

### ACCTFILE-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed VSAM file (ORGANIZATION INDEXED, ACCESS SEQUENTIAL, KEY FD-ACCT-ID) of account records read into ACCOUNT-RECORD from CVACT01Y
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 29, 30, 31, 32, 166

### CODATECN-REC
- **Type:** PARAMETER
- **Description:** Date conversion fields from CODATECN copybook used for input/output date formatting via COBDATFT
- **Copybook:** [CODATECN](../copybooks/CODATECN.md)
- **Lines:** 90, 223, 231

## Outputs

### OUT-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential fixed-length account records (OUT-ACCT-REC inline FD lines 57-70) with transformed reissue date and conditional debit override
- **Copybook:** [OUT-ACCT-REC (inline FD)](../copybooks/OUT-ACCT-REC (INLINE FD).md)
- **Lines:** 243

### ARRY-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential array records (ARR-ARRAY-REC inline FD lines 72-78) with account ID and 5 repeating balance/debit groups, some hardcoded
- **Copybook:** [ARR-ARRAY-REC (inline FD)](../copybooks/ARR-ARRAY-REC (INLINE FD).md)
- **Lines:** 264

### VBRC-FILE
- **Type:** FILE_SEQUENTIAL
- **Description:** Variable-length sequential records (VBR-REC inline FD lines 81-85, varying 10-80 via WS-RECD-LEN) with two types per account: short VB1 (12 bytes) and long VB2 (39 bytes)
- **Copybook:** [VBR-REC (inline FD)](../copybooks/VBR-REC (INLINE FD).md)
- **Lines:** 290, 305

### SYSPRINT
- **Type:** REPORT
- **Description:** Console output displaying execution start/end, account details per record, I/O error statuses
- **Lines:** 141, 151, 201, 283, 407, 413

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [COBDATFT](./COBDATFT.md) | STATIC_CALL | Assembler routine to format CODATECN-INP-DATE to CODATECN-OUT-DATE for account reissue date | 231 |
| [CEE3ABD](./CEE3ABD.md) | STATIC_CALL | Abend the program with custom return code ABCODE on I/O errors | 410 |

## Business Rules

### BR001: Override current cycle debit to 2525.00 if input is zero
**Logic:** IF ACCT-CURR-CYC-DEBIT EQUAL TO ZERO MOVE 2525.00 TO OUT-ACCT-CURR-CYC-DEBIT
**Conditions:** IF ACCT-CURR-CYC-DEBIT EQUAL TO ZERO
**Lines:** 236, 237

### BR002: Format reissue date using COBDATFT before writing to OUT-FILE
**Logic:** MOVE ACCT-REISSUE-DATE TO CODATECN-INP-DATE WS-REISSUE-DATE, set CODATECN-TYPE/OUTTYPE '2', CALL COBDATFT, MOVE CODATECN-OUT-DATE TO OUT-ACCT-REISSUE-DATE
**Conditions:** Always during populate
**Lines:** 223, 224, 225, 226, 231, 233

### BR003: Populate ARRY-FILE array with hardcoded values for balances/debits in elements 1-3
**Logic:** Copy ACCT-CURR-BAL to (1) and (2), hardcode CURR-CYC-DEBIT(1)=1005.00, (2)=1525.00; CURR-BAL(3)=-1025.00, DEBIT(3)=-2500.00
**Conditions:** Always during populate
**Lines:** 255, 256, 257, 258, 259, 260

### BR004: Generate two variable records per account for VBRC-FILE: VB1 (ID+status, len=12), VB2 (ID+bal+limit+reissue YYYY, len=39)
**Logic:** Populate VBRC-REC1/REC2 from account fields including WS-ACCT-REISSUE-YYYY, set WS-RECD-LEN, MOVE substring to VBR-REC(1:len), WRITE
**Conditions:** Always during process
**Lines:** 276, 282, 288, 303

### BR005: Detect end-of-file on ACCTFILE and stop processing loop
**Logic:** If READ status='10' set APPL-EOF (16), then if APPL-EOF MOVE 'Y' TO END-OF-FILE to exit UNTIL loop
**Conditions:** IF ACCTFILE-STATUS = '10'
**Lines:** 180, 190

### BR006: Display all account record fields to SYSPRINT for each processed record
**Logic:** DISPLAY each field (ID, status, balances, dates, etc.) with labels in 1100-DISPLAY-ACCT-RECORD
**Conditions:** After successful READ
**Lines:** 170, 200, 212

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVACT01Y](../copybooks/CVACT01Y.md) | WORKING_STORAGE | Defines input ACCOUNT-RECORD structure with account fields (ACCT-ID etc.) | 89 |
| [CODATECN](../copybooks/CODATECN.md) | WORKING_STORAGE | Defines CODATECN-REC fields for date input/output and types used by COBDATFT | 90 |
| [OUT-ACCT-REC](../copybooks/OUT-ACCT-REC.md) | FILE_SECTION | Inline FD defines fixed sequential output record structure for OUT-FILE | 57 |
| [ARR-ARRAY-REC](../copybooks/ARR-ARRAY-REC.md) | FILE_SECTION | Inline FD defines array output record with OCCURS 5 for ARRY-FILE | 72 |
| [VBR-REC](../copybooks/VBR-REC.md) | FILE_SECTION | Inline FD defines variable-length PIC X(80) record for VBRC-FILE with RECD-LEN | 85 |

## Data Flow

### Reads From
- **ACCTFILE-FILE**: ACCT-ID, ACCT-ACTIVE-STATUS, ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, ACCT-OPEN-DATE, ACCT-EXPIRAION-DATE, ACCT-REISSUE-DATE, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT, ACCT-GROUP-ID
  (Lines: 166, 201, 216)

### Writes To
- **OUT-FILE**: OUT-ACCT-ID, OUT-ACCT-ACTIVE-STATUS, OUT-ACCT-CURR-BAL, OUT-ACCT-CREDIT-LIMIT, OUT-ACCT-CASH-CREDIT-LIMIT, OUT-ACCT-OPEN-DATE, OUT-ACCT-EXPIRAION-DATE, OUT-ACCT-REISSUE-DATE, OUT-ACCT-CURR-CYC-CREDIT, OUT-ACCT-CURR-CYC-DEBIT, OUT-ACCT-GROUP-ID
  (Lines: 243)
- **ARRY-FILE**: ARR-ACCT-ID, ARR-ACCT-CURR-BAL (1-3), ARR-ACCT-CURR-CYC-DEBIT (1-3)
  (Lines: 264)
- **VBRC-FILE**: VB1-ACCT-ID, VB1-ACCT-ACTIVE-STATUS, VB2-ACCT-ID, VB2-ACCT-CURR-BAL, VB2-ACCT-CREDIT-LIMIT, VB2-ACCT-REISSUE-YYYY
  (Lines: 290, 305)
- **SYSPRINT**: ACCT-ID, ACCT-ACTIVE-STATUS, ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, ACCT-OPEN-DATE, ACCT-EXPIRAION-DATE, ACCT-REISSUE-DATE, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT, ACCT-GROUP-ID, VBRC-REC1, VBRC-REC2
  (Lines: 141, 151, 201, 283)

### Transformations
- **ACCT-REISSUE-DATE** → **OUT-ACCT-REISSUE-DATE**: Passed through CODATECN-INP-DATE and COBDATFT (type '2') to CODATECN-OUT-DATE
  (Lines: 223, 231, 233)
- **ACCT-CURR-CYC-DEBIT** → **OUT-ACCT-CURR-CYC-DEBIT**: Direct copy unless zero, then overridden to 2525.00
  (Lines: 235, 236, 237)
- **ACCT-CURR-BAL** → **ARR-ACCT-CURR-BAL(1)**: Direct copy to array index 1 (similarly index 2)
  (Lines: 255)
- **WS-ACCT-REISSUE-YYYY** → **VB2-ACCT-REISSUE-YYYY**: Extracted YYYY portion after date formatting for VB2 record
  (Lines: 282)

## Key Paragraphs

### 1000-ACCTFILE-GET-NEXT
**Purpose:** Core loop logic: READ input, check status/EOF, chain performs to display/populate/write all outputs or error/abend
- Calls: 1100-DISPLAY-ACCT-RECORD, 1300-POPUL-ACCT-RECORD, 1350-WRITE-ACCT-RECORD, 1400-POPUL-ARRAY-RECORD, 1450-WRITE-ARRY-RECORD, 1500-POPUL-VBRC-RECORD, 1550-WRITE-VB1-RECORD, 1575-WRITE-VB2-RECORD, 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 165-197

### 0000-ACCTFILE-OPEN
**Purpose:** Open input ACCTFILE-FILE for sequential read, abend on error
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 317-332

### 2000-OUTFILE-OPEN
**Purpose:** Open output OUT-FILE, abend on error
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 334-349

### 3000-ARRFILE-OPEN
**Purpose:** Open output ARRY-FILE, abend on error
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 352-367

### 4000-VBRFILE-OPEN
**Purpose:** Open output VBRC-FILE, abend on error
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 370-385

### 9000-ACCTFILE-CLOSE
**Purpose:** Close input ACCTFILE-FILE, abend on error
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 388-403

### 9999-ABEND-PROGRAM
**Purpose:** Terminate program via CEE3ABD call with abend code
- Lines: 406-412

### 9910-DISPLAY-IO-STATUS
**Purpose:** Format and display file status code as numeric NNNN
- Lines: 413-425

### 1100-DISPLAY-ACCT-RECORD
**Purpose:** Display all fields of current ACCOUNT-RECORD with labels
- Lines: 200-212

## Error Handling

- **ACCTFILE-STATUS NOT = '00' or '10' after READ:** DISPLAY error, PERFORM 9910-DISPLAY-IO-STATUS then 9999-ABEND-PROGRAM
  (Lines: 192, 194, 195)
- **OUTFILE-STATUS / ARRYFILE-STATUS / VBRCFILE-STATUS NOT = '00' AND NOT = '10' after WRITE:** DISPLAY status, PERFORM 9910-DISPLAY-IO-STATUS then 9999-ABEND-PROGRAM
  (Lines: 245, 248, 249, 266, 271, 272, 292, 297, 298, 307, 312, 313)
- **File status NOT = '00' after OPEN / CLOSE:** DISPLAY error message with status, PERFORM 9910-DISPLAY-IO-STATUS then 9999-ABEND-PROGRAM
  (Lines: 328, 330, 331, 345, 347, 348, 363, 365, 366, 381, 383, 384, 399, 401, 402)

## Open Questions

- **Exact field layouts in CVACT01Y copybook for ACCOUNT-RECORD**
  - Context: Fields like ACCT-ID referenced extensively but definitions not in source
  - Suggestion: Obtain and analyze CVACT01Y source
- **Details of CODATECN copybook fields and COBDATFT input/output formats for type '2'**
  - Context: Fields like CODATECN-INP-DATE used but not defined here
  - Suggestion: Review CODATECN copybook and COBDATFT specs
- **Business intent of hardcoded values in ARRY-FILE (1005.00, 1525.00, -1025.00, -2500.00)**
  - Context: No conditional logic; likely demo/test data
  - Suggestion: Consult CardDemo requirements documentation

---
*Generated by War Rig WAR_RIG*