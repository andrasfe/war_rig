# CBSTM03B

**File:** CBSTM03B.CBL
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 2
**Analyzed:** 2026-01-18 17:52:14.786502

## Purpose

This batch COBOL subroutine provides file access services limited to OPEN INPUT ('O'), sequential READ ('R') for TRNX-FILE/XREF-FILE, keyed READ ('K') for CUST-FILE/ACCT-FILE, and CLOSE ('C') based on linkage parameters for file DD name and operation. It loads read record data into linkage LK-M03B-FLDT and returns file status in LK-M03B-RC. Unrecognized DD names or operations (including defined but unused WRITE 'W'/REWRITE 'Z') result in no action and return.

**Business Context:** Part of CardDemo application for transaction report file processing
**Program Type:** SUBROUTINE
**Citations:** Lines 2, 5, 6, 7, 8, 26, 103, 104, 105, 106, 107, 108, 114, 116, 118

## Calling Context

**Linkage Section:** LK-M03B-AREA, LK-M03B-DD, LK-M03B-OPER, LK-M03B-RC, LK-M03B-KEY, LK-M03B-KEY-LN, LK-M03B-FLDT

## Inputs

### LK-M03B-AREA
- **Type:** PARAMETER
- **Description:** Linkage structure providing file DD name (LK-M03B-DD), operation code (LK-M03B-OPER with 88s O/C/R/K/W/Z), optional key and length (LK-M03B-KEY/LK-M03B-KEY-LN for keyed reads)
- **Lines:** 99, 101, 114, 118, 135, 140, 164, 183, 188, 213

### TRNX-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed file (ORGANIZATION INDEXED, ACCESS SEQUENTIAL, RECORD KEY FD-TRNXS-ID composed of FD-TRNX-CARD PIC X(16) + FD-TRNX-ID PIC X(16)) with account data FD-ACCT-DATA PIC X(318)
- **Lines:** 141

### XREF-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed file (ORGANIZATION INDEXED, ACCESS SEQUENTIAL, RECORD KEY FD-XREF-CARD-NUM PIC X(16)) with data FD-XREF-DATA PIC X(34)
- **Lines:** 165

### CUST-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed file (ORGANIZATION INDEXED, ACCESS RANDOM, RECORD KEY FD-CUST-ID PIC X(09)) with customer data FD-CUST-DATA PIC X(491)
- **Lines:** 190

### ACCT-FILE
- **Type:** FILE_VSAM
- **Description:** Indexed file (ORGANIZATION INDEXED, ACCESS RANDOM, RECORD KEY FD-ACCT-ID PIC 9(11)) with account data FD-ACCT-DATA PIC X(289)
- **Lines:** 215

## Outputs

### LK-M03B-FLDT
- **Type:** PARAMETER
- **Description:** Receives entire record data from file on successful READ or READ-K operations
- **Lines:** 141, 165, 190, 215

### LK-M03B-RC
- **Type:** PARAMETER
- **Description:** File status code (2 bytes) set after each file operation or on fall-through
- **Lines:** 152, 176, 201, 226

## Business Rules

### BR001: Select file processing routine based on file DD name in LK-M03B-DD
**Logic:** EVALUATE LK-M03B-DD to PERFORM specific PROC THRU EXIT
**Conditions:** LK-M03B-DD = 'TRNXFILE', LK-M03B-DD = 'XREFFILE', LK-M03B-DD = 'CUSTFILE', LK-M03B-DD = 'ACCTFILE'
**Lines:** 118, 120, 122, 124, 126

### BR002: Execute OPEN INPUT if operation is 'O' (M03B-OPEN)
**Logic:** IF M03B-OPEN OPEN INPUT file-name; GOTO exit
**Conditions:** M03B-OPEN (LK-M03B-OPER = 'O')
**Lines:** 135, 159, 183, 208

### BR003: Execute sequential READ if operation is 'R' (M03B-READ) for TRNX-FILE or XREF-FILE
**Logic:** IF M03B-READ READ file INTO LK-M03B-FLDT; GOTO exit
**Conditions:** M03B-READ (LK-M03B-OPER = 'R')
**Lines:** 140, 164

### BR004: Execute keyed READ if operation is 'K' (M03B-READ-K) for CUST-FILE or ACCT-FILE
**Logic:** IF M03B-READ-K MOVE key substring to file key; READ file INTO LK-M03B-FLDT; GOTO exit
**Conditions:** M03B-READ-K (LK-M03B-OPER = 'K')
**Lines:** 188, 213

### BR005: Execute CLOSE if operation is 'C' (M03B-CLOSE)
**Logic:** IF M03B-CLOSE CLOSE file; GOTO exit
**Conditions:** M03B-CLOSE (LK-M03B-OPER = 'C')
**Lines:** 146, 170, 195, 220

### BR006: Unrecognized DD name or unhandled operation (not O/R/K/C, e.g. defined W/Z) results in no action
**Logic:** WHEN OTHER GO TO GOBACK for DD; unhandled OPER falls through IFs to EXIT with status move (no operation performed)
**Conditions:** LK-M03B-DD not one of four files, LK-M03B-OPER not O/R/K/C
**Lines:** 127, 128, 151, 153, 175, 177, 200, 202, 225, 227

## Data Flow

### Reads From
- **LK-M03B-AREA**: LK-M03B-DD, LK-M03B-OPER, LK-M03B-KEY, LK-M03B-KEY-LN
  (Lines: 118, 135, 140, 164, 183, 188, 213)
- **TRNX-FILE**: FD-TRNXFILE-REC
  (Lines: 141)
- **XREF-FILE**: FD-XREFFILE-REC
  (Lines: 165)
- **CUST-FILE**: FD-CUSTFILE-REC
  (Lines: 190)
- **ACCT-FILE**: FD-ACCTFILE-REC
  (Lines: 215)

### Writes To
- **LK-M03B-FLDT**: LK-M03B-FLDT
  (Lines: 141, 165, 190, 215)
- **LK-M03B-RC**: LK-M03B-RC
  (Lines: 152, 176, 201, 226)
- **FD-CUST-ID**: FD-CUST-ID
  (Lines: 189)
- **FD-ACCT-ID**: FD-ACCT-ID
  (Lines: 214)

### Transformations
- **LK-M03B-KEY (1:LK-M03B-KEY-LN)** → **FD-CUST-ID**: Substring move from linkage key to file record key for random access read
  (Lines: 189)
- **LK-M03B-KEY (1:LK-M03B-KEY-LN)** → **FD-ACCT-ID**: Substring move from linkage key to file record key for random access read
  (Lines: 214)
- **FD-TRNXFILE-REC** → **LK-M03B-FLDT**: Direct transfer of entire record via READ INTO
  (Lines: 141)
- **FD-XREFFILE-REC** → **LK-M03B-FLDT**: Direct transfer of entire record via READ INTO
  (Lines: 165)
- **FD-CUSTFILE-REC** → **LK-M03B-FLDT**: Direct transfer of entire record via READ INTO after key move
  (Lines: 190)
- **FD-ACCTFILE-REC** → **LK-M03B-FLDT**: Direct transfer of entire record via READ INTO after key move
  (Lines: 215)
- **TRNXFILE-STATUS** → **LK-M03B-RC**: Direct move of file status code
  (Lines: 152)
- **XREFFILE-STATUS** → **LK-M03B-RC**: Direct move of file status code
  (Lines: 176)
- **CUSTFILE-STATUS** → **LK-M03B-RC**: Direct move of file status code
  (Lines: 201)
- **ACCTFILE-STATUS** → **LK-M03B-RC**: Direct move of file status code
  (Lines: 226)

## Key Paragraphs

### 0000-START
**Purpose:** Entry point: EVALUATE linkage DD name to invoke file-specific processing thru exit or GOBACK
- Calls: 1000-TRNXFILE-PROC THRU 1999-EXIT, 2000-XREFFILE-PROC THRU 2999-EXIT, 3000-CUSTFILE-PROC THRU 3999-EXIT, 4000-ACCTFILE-PROC THRU 4999-EXIT
- Lines: 116-129

### 1000-TRNXFILE-PROC
**Purpose:** Handles conditional OPEN/READ/CLOSE or fall-through for TRNX-FILE
- Called by: 0000-START
- Calls: 1900-EXIT
- Lines: 133-150

### 1900-EXIT
**Purpose:** Moves TRNXFILE-STATUS to LK-M03B-RC
- Called by: 1000-TRNXFILE-PROC
- Calls: 1999-EXIT
- Lines: 151-153

### 2000-XREFFILE-PROC
**Purpose:** Handles conditional OPEN/READ/CLOSE or fall-through for XREF-FILE
- Called by: 0000-START
- Calls: 2900-EXIT
- Lines: 157-174

### 2900-EXIT
**Purpose:** Moves XREFFILE-STATUS to LK-M03B-RC
- Called by: 2000-XREFFILE-PROC
- Calls: 2999-EXIT
- Lines: 175-177

### 3000-CUSTFILE-PROC
**Purpose:** Handles conditional OPEN/READ-K/CLOSE or fall-through for CUST-FILE
- Called by: 0000-START
- Calls: 3900-EXIT
- Lines: 181-199

### 3900-EXIT
**Purpose:** Moves CUSTFILE-STATUS to LK-M03B-RC
- Called by: 3000-CUSTFILE-PROC
- Calls: 3999-EXIT
- Lines: 200-202

### 4000-ACCTFILE-PROC
**Purpose:** Handles conditional OPEN/READ-K/CLOSE or fall-through for ACCT-FILE
- Called by: 0000-START
- Calls: 4900-EXIT
- Lines: 206-224

### 4900-EXIT
**Purpose:** Moves ACCTFILE-STATUS to LK-M03B-RC
- Called by: 4000-ACCTFILE-PROC
- Calls: 4999-EXIT
- Lines: 225-227

### 9999-GOBACK
**Purpose:** Program termination for unrecognized DD
- Called by: 0000-START
- Lines: 130-132

## Error Handling

- **Any file status after OPEN INPUT, READ, READ-K, CLOSE or no operation (fall-through):** Status unconditionally propagated to LK-M03B-RC without checks, abends, or logic; caller must interpret (typical VSAM: '00' success, '10' no current record/EOF, '23' record not found, '97' record locked)
  (Lines: 152, 176, 201, 226)

## Open Questions

- **No copybooks used**
  - Context: Preprocessor analysis reports empty copybooks list; no COPY statements in source
  - Suggestion: Confirmed; no further action needed
- **What program(s) call this subroutine and how (static/dynamic CALL)?**
  - Context: Not determinable from this source; comment mentions 'called by the statement create program'
  - Suggestion: Analyze JCL/PROC or main program source
- **Why are M03B-WRITE ('W') and M03B-REWRITE ('Z') 88 conditions defined but not implemented?**
  - Context: 88 levels defined in LINKAGE but no corresponding IF logic; program performs only O/R/K/C
  - Suggestion: Check calling program or requirements spec

---
*Generated by War Rig WAR_RIG*