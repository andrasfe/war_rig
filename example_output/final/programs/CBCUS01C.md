# CBCUS01C

**File:** CBCUS01C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 2
**Analyzed:** 2026-01-18 17:08:58.553970

## Purpose

Batch COBOL program that opens a VSAM indexed customer file (CUSTFILE-FILE) for input, reads records sequentially until end-of-file using READ NEXT INTO CUSTOMER-RECORD, displays each successful customer record via DISPLAY, and closes the file. Handles I/O errors (status not '00' or '10') by displaying error messages and status, then abending the program. Part of CardDemo application; CUSTOMER-RECORD structure defined in unprovided copybook CVCUS01Y and displayed verbatim.

**Business Context:** CardDemo application for processing customer data
**Program Type:** BATCH
**Citations:** Lines 2, 3, 4, 5, 71, 85

## Inputs

### CUSTFILE-FILE
- **Type:** FILE_VSAM
- **Description:** VSAM KSDS file with ORGANIZATION IS INDEXED, ACCESS MODE IS SEQUENTIAL, RECORD KEY FD-CUST-ID (PIC 9(09)), record layout FD-CUSTFILE-REC with FD-CUST-ID and FD-CUST-DATA (PIC X(491)); records read sequentially into CUSTOMER-RECORD
- **Copybook:** [CVCUS01Y](../copybooks/CVCUS01Y.md)
- **Lines:** 29, 30, 31, 32, 38, 39, 40, 45, 93, 120

## Outputs

### DISPLAY OUTPUT
- **Type:** REPORT
- **Description:** Execution start/end messages ('START OF EXECUTION...', 'END OF EXECUTION...'), successful CUSTOMER-RECORD displayed verbatim (structure from CVCUS01Y copybook), error messages ('ERROR READING/OPENING/CLOSING...'), formatted file IO-STATUS ('FILE STATUS IS: NNNN'), and abend message ('ABENDING PROGRAM')
- **Copybook:** [CVCUS01Y](../copybooks/CVCUS01Y.md)
- **Lines:** 71, 78, 85, 96, 110, 129, 147, 155, 162, 168, 172

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CEE3ABD](./CEE3ABD.md) | STATIC_CALL | Abends the program with user abend code in ABCODE (set to 999) and timing info (set to 0) | 158 |

## Business Rules

### BR001: Treat file status '00' as successful I/O and continue processing
**Logic:** If CUSTFILE-STATUS = '00' then MOVE 0 TO APPL-RESULT and display record if READ
**Conditions:** CUSTFILE-STATUS = '00'
**Lines:** 94, 95, 96, 121, 122, 139, 140

### BR002: Treat file status '10' as end-of-file on READ
**Logic:** If CUSTFILE-STATUS = '10' then MOVE 16 TO APPL-RESULT and set END-OF-FILE = 'Y' if APPL-EOF
**Conditions:** CUSTFILE-STATUS = '10', APPL-EOF
**Lines:** 98, 99, 107, 108

### BR003: On any I/O error (status not '00' or '10'), display error message, IO status, and abend program
**Logic:** MOVE 12 TO APPL-RESULT, DISPLAY error, PERFORM Z-DISPLAY-IO-STATUS and Z-ABEND-PROGRAM
**Conditions:** APPL-RESULT NOT APPL-AOK or APPL-EOF
**Lines:** 101, 110, 111, 112, 113, 124, 129, 130, 131, 132, 142, 147, 148, 149, 150

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVCUS01Y](../copybooks/CVCUS01Y.md) | WORKING_STORAGE | Defines CUSTOMER-RECORD data structure matching file record layout, used for READ INTO and DISPLAY | 45 |

## Data Flow

### Reads From
- **CUSTFILE-FILE**: FD-CUST-ID, FD-CUST-DATA
  (Lines: 93)

### Writes To
- **DISPLAY OUTPUT**: CUSTOMER-RECORD
  (Lines: 78, 96)

### Transformations
- **FD-CUSTFILE-REC** → **CUSTOMER-RECORD**: Direct move via READ CUSTFILE-FILE INTO CUSTOMER-RECORD (no field-level transforms observed)
  (Lines: 93)

## Key Paragraphs

### MAIN
**Purpose:** Unnamed main control flow in PROCEDURE DIVISION: displays start message, PERFORM 0000-CUSTFILE-OPEN, loop PERFORM UNTIL EOF with nested PERFORM 1000-CUSTFILE-GET-NEXT and DISPLAY CUSTOMER-RECORD, PERFORM 9000-CUSTFILE-CLOSE, display end message, GOBACK
- Calls: 0000-CUSTFILE-OPEN, 1000-CUSTFILE-GET-NEXT, 9000-CUSTFILE-CLOSE
- Lines: 70-91

### END-PERFORM
**Purpose:** Labeled END-PERFORM terminating the main UNTIL loop (lines 74-81)
- Lines: 81-91

### 0-CUSTFILE-GET-NEXT
**Purpose:** Performs sequential READ CUSTFILE-FILE INTO CUSTOMER-RECORD, handles status '00' (success/display), '10' (EOF), other (error/display/abend)
- Calls: Z-DISPLAY-IO-STATUS, Z-ABEND-PROGRAM
- Lines: 92-115

### 0-CUSTFILE-OPEN
**Purpose:** Sets APPL-RESULT=8, OPEN INPUT CUSTFILE-FILE, checks status '00' (success) else error/display/abend
- Calls: Z-DISPLAY-IO-STATUS, Z-ABEND-PROGRAM
- Lines: 118-133

### 0-CUSTFILE-CLOSE
**Purpose:** Sets APPL-RESULT=8, CLOSE CUSTFILE-FILE, checks status '00' (success) else error/display/abend
- Calls: Z-DISPLAY-IO-STATUS, Z-ABEND-PROGRAM
- Lines: 136-151

### Z-ABEND-PROGRAM
**Purpose:** Displays 'ABENDING PROGRAM', sets ABCODE=999, TIMING=0, static CALL 'CEE3ABD'
- Called by: 0-CUSTFILE-GET-NEXT, 0-CUSTFILE-OPEN, 0-CUSTFILE-CLOSE
- Lines: 154-160

### Z-DISPLAY-IO-STATUS
**Purpose:** Formats and DISPLAYs IO-STATUS as numeric 'NNNN' handling non-numeric or '9x' cases using redefines and binary conversion
- Called by: 0-CUSTFILE-GET-NEXT, 0-CUSTFILE-OPEN, 0-CUSTFILE-CLOSE
- Lines: 161-173

## Error Handling

- **CUSTFILE-STATUS NOT = '00' on READ (except '10'):** DISPLAY 'ERROR READING CUSTOMER FILE', MOVE CUSTFILE-STATUS TO IO-STATUS, PERFORM Z-DISPLAY-IO-STATUS, PERFORM Z-ABEND-PROGRAM
  (Lines: 110, 111, 112, 113)
- **CUSTFILE-STATUS NOT = '00' on OPEN:** DISPLAY 'ERROR OPENING CUSTFILE', MOVE CUSTFILE-STATUS TO IO-STATUS, PERFORM Z-DISPLAY-IO-STATUS, PERFORM Z-ABEND-PROGRAM
  (Lines: 129, 130, 131, 132)
- **CUSTFILE-STATUS NOT = '00' on CLOSE:** DISPLAY 'ERROR CLOSING CUSTOMER FILE', MOVE CUSTFILE-STATUS TO IO-STATUS, PERFORM Z-DISPLAY-IO-STATUS, PERFORM Z-ABEND-PROGRAM
  (Lines: 147, 148, 149, 150)

## Open Questions

- **Detailed field layout of CUSTOMER-RECORD**
  - Context: Defined in unprovided copybook CVCUS01Y at line 45
  - Suggestion: Obtain and analyze copybook CVCUS01Y
- **Exact behavior of CEE3ABD with ABCODE=999 and TIMING=0**
  - Context: LE abend routine called statically; specific effects unknown without LE docs
  - Suggestion: Consult IBM Language Environment documentation for CEE3ABD

---
*Generated by War Rig WAR_RIG*