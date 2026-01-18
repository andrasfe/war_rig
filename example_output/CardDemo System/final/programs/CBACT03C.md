# CBACT03C

**File:** CBACT03C.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 2
**Analyzed:** 2026-01-18 16:49:35.747062

## Purpose

Batch COBOL program that opens VSAM KSDS file XREFFILE-FILE for input, then in a PERFORM UNTIL END-OF-FILE = 'Y' loop performs 1000-XREFFILE-GET-NEXT to sequentially read records into CARD-XREF-RECORD and conditionally displays each successful read (if END-OF-FILE = 'N'), until EOF is detected. Closes the file, displays end message, and GOBACKs, abending via CEE3ABD on any I/O errors after displaying status.

**Business Context:** CardDemo application: Read and print account cross reference data file (line 5).
**Program Type:** BATCH
**Citations:** Lines 5, 71, 72, 74, 76, 78, 83, 85, 93

## Inputs

### XREFFILE-FILE
- **Type:** FILE_VSAM
- **Description:** VSAM KSDS (ORGANIZATION IS INDEXED, ACCESS MODE IS SEQUENTIAL, RECORD KEY FD-XREF-CARD-NUM PIC X(16)) file of account cross reference data records with data FD-XREF-DATA PIC X(34), read sequentially into CARD-XREF-RECORD
- **Copybook:** [CVACT03Y](../copybooks/CVACT03Y.md)
- **Lines:** 29, 30, 31, 32, 37, 39, 40, 93

## Outputs

### CONSOLE
- **Type:** REPORT
- **Description:** Displays each CARD-XREF-RECORD after successful read if not EOF, plus execution start/end messages, error messages, and detailed file status on errors
- **Copybook:** [CVACT03Y](../copybooks/CVACT03Y.md)
- **Lines:** 71, 78, 85, 96, 110, 129, 147, 162

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CEE3ABD](./CEE3ABD.md) | STATIC_CALL | Abends the program with user code 999 (ABCODE) and timing 0 (TIMING) on I/O errors | 158 |

## Business Rules

### BR001: Treat file status '10' on READ as end-of-file
**Logic:** Set APPL-RESULT to 16 if status '10', then if APPL-EOF move 'Y' to END-OF-FILE
**Conditions:** XREFFILE-STATUS = '10'
**Lines:** 98, 99, 107, 108

### BR002: Abend program on READ error (status not '00' or '10')
**Logic:** Display error message, move status to IO-STATUS, perform display IO status, perform abend
**Conditions:** NOT APPL-AOK, NOT APPL-EOF
**Lines:** 104, 110, 111, 112, 113

### BR003: Abend program on OPEN INPUT error (status not '00')
**Logic:** Display error message, move status to IO-STATUS, perform display IO status, perform abend
**Conditions:** NOT APPL-AOK
**Lines:** 121, 128, 129, 130, 131, 132

### BR004: Abend program on CLOSE error (status not '00')
**Logic:** Display error message, move status to IO-STATUS, perform display IO status, perform abend
**Conditions:** NOT APPL-AOK
**Lines:** 139, 146, 147, 148, 149, 150

### BR005: On successful READ (status '00'), set APPL-RESULT to 0 and display the record
**Logic:** MOVE 0 TO APPL-RESULT; DISPLAY CARD-XREF-RECORD
**Conditions:** XREFFILE-STATUS = '00'
**Lines:** 94, 95, 96

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVACT03Y](../copybooks/CVACT03Y.md) | WORKING_STORAGE | Defines CARD-XREF-RECORD working record area used for READ INTO and DISPLAY of cross reference data | 45 |

## Data Flow

### Reads From
- **XREFFILE-FILE**: CARD-XREF-RECORD
  (Lines: 93)

### Writes To
- **CONSOLE**: CARD-XREF-RECORD, execution start/end messages, error messages, IO-STATUS formatted
  (Lines: 71, 78, 85, 96, 110, 129, 147, 162)

## Key Paragraphs

### 0-XREFFILE-GET-NEXT
**Purpose:** Sequentially reads next record from XREFFILE-FILE into CARD-XREF-RECORD, handles status '00' (continue/display), '10' (set EOF), other (error/abend)
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 92-115

### 0-XREFFILE-OPEN
**Purpose:** Opens XREFFILE-FILE for INPUT, sets APPL-RESULT based on status, abends on error
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 118-133

### 0-XREFFILE-CLOSE
**Purpose:** Closes XREFFILE-FILE, sets APPL-RESULT based on status, abends on error
- Calls: 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
- Lines: 136-151

### 9-ABEND-PROGRAM
**Purpose:** Displays abend message and calls CEE3ABD to terminate program abnormally
- Lines: 154-160

### 0-DISPLAY-IO-STATUS
**Purpose:** Formats and displays file IO-STATUS in numeric 'NNNN' format handling non-numeric cases
- Lines: 161-173

### MAIN
**Purpose:** Main control: display start, open file, loop PERFORM 1000-XREFFILE-GET-NEXT with DISPLAY until EOF='Y', close file, display end, GOBACK
- Calls: 0000-XREFFILE-OPEN, 1000-XREFFILE-GET-NEXT, 9000-XREFFILE-CLOSE
- Lines: 70-87

## Error Handling

- **XREFFILE-STATUS NOT = '00' OR '10' AFTER READ:** DISPLAY 'ERROR READING XREFFILE', PERFORM 9910-DISPLAY-IO-STATUS, PERFORM 9999-ABEND-PROGRAM
  (Lines: 110, 111, 112, 113)
- **XREFFILE-STATUS NOT = '00' AFTER OPEN INPUT:** DISPLAY 'ERROR OPENING XREFFILE', PERFORM 9910-DISPLAY-IO-STATUS, PERFORM 9999-ABEND-PROGRAM
  (Lines: 129, 130, 131, 132)
- **XREFFILE-STATUS NOT = '00' AFTER CLOSE:** DISPLAY 'ERROR CLOSING XREFFILE', PERFORM 9910-DISPLAY-IO-STATUS, PERFORM 9999-ABEND-PROGRAM
  (Lines: 147, 148, 149, 150)

## Open Questions

- **Detailed field structure of CARD-XREF-RECORD**
  - Context: Defined in unprovided copybook CVACT03Y; only used as whole record
  - Suggestion: Analyze COPYBOOK CVACT03Y
- **Precise business meaning of XREFFILE cross reference data**
  - Context: Described as 'account cross reference data file' but fields beyond key/data lengths unknown without copybook
  - Suggestion: Review CardDemo application documentation

---
*Generated by War Rig WAR_RIG*