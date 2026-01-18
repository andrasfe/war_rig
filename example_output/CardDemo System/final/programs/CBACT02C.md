# CBACT02C

**File:** CBACT02C.cbl
**Type:** COBOL
**Status:** FinalStatus.FORCED
**Iterations:** 2
**Analyzed:** 2026-01-18 16:21:02.514977

## Purpose

This batch COBOL program displays start message, opens VSAM KSDS CARDFILE-FILE for input, loops performing sequential reads into CARD-RECORD until end-of-file status '10' sets END-OF-FILE='Y', displaying CARD-RECORD only if END-OF-FILE='N' after each read, then closes the file and displays end message. It handles I/O errors on open/read/close by displaying error messages and status, then abending via CEE3ABD.

**Business Context:** CardDemo application: reads card data from VSAM file and prints (displays) records sequentially (lines 3-5)
**Program Type:** BATCH
**Citations:** Lines 2, 3, 4, 5, 71, 72, 74, 75, 76, 77, 78, 80, 83, 85, 93

## Inputs

### CARDFILE-FILE
- **Type:** FILE_VSAM
- **Description:** VSAM KSDS (INDEXED ORGANIZATION, SEQUENTIAL ACCESS) file of card data records with RECORD KEY FD-CARD-NUM (defined but unused for positioning in this sequential read from start); record layout FD-CARDFILE-REC: FD-CARD-NUM PIC X(16), FD-CARD-DATA PIC X(134); read INTO CARD-RECORD
- **Copybook:** [CVACT02Y](../copybooks/CVACT02Y.md)
- **Lines:** 29, 30, 31, 32, 37, 38, 39, 40, 93

## Outputs

### CONSOLE
- **Type:** REPORT
- **Description:** Displays program execution start/end messages, successful CARD-RECORD contents (conditionally), all error messages ('ERROR READING/OPENING/CLOSING CARDFILE', 'ABENDING PROGRAM'), and formatted file I/O status codes
- **Lines:** 71, 78, 85, 110, 129, 147, 155, 168, 172

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CEE3ABD](./CEE3ABD.md) | STATIC_CALL | Abends the program with user-specified return code ABCODE and timing information | 158 |

## Business Rules

### BR001: Treat file status '00' as successful read
**Logic:** MOVE 0 TO APPL-RESULT if CARDFILE-STATUS = '00'
**Conditions:** CARDFILE-STATUS = '00'
**Lines:** 94, 95

### BR002: Treat file status '10' as end-of-file: set APPL-RESULT=16 to trigger APPL-EOF condition 88 level, which then sets END-OF-FILE='Y' to exit loop
**Logic:** MOVE 16 TO APPL-RESULT if CARDFILE-STATUS = '10'; later IF APPL-EOF MOVE 'Y' TO END-OF-FILE
**Conditions:** CARDFILE-STATUS = '10', APPL-EOF
**Lines:** 98, 99, 107, 108

### BR003: Treat non-'00'/'10' status as read error
**Logic:** MOVE 12 TO APPL-RESULT, display error, show status, abend if not EOF
**Conditions:** CARDFILE-STATUS NOT = '00' OR '10', NOT APPL-EOF
**Lines:** 101, 110, 111, 112, 113

### BR004: Abend on open or close errors similar to read errors
**Logic:** MOVE 12 TO APPL-RESULT if status not '00', display error, show status, abend
**Conditions:** CARDFILE-STATUS NOT = '00'
**Lines:** 124, 129, 142, 147

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVACT02Y](../copybooks/CVACT02Y.md) | WORKING_STORAGE | Defines CARD-RECORD working storage item used for reading file records and display | 45 |

## Data Flow

### Reads From
- **CARDFILE-FILE**: FD-CARDFILE-REC
  (Lines: 93)

### Writes To
- **CONSOLE**: CARD-RECORD, 'START OF EXECUTION OF PROGRAM CBACT02C', 'END OF EXECUTION OF PROGRAM CBACT02C', 'ERROR READING CARDFILE', 'ERROR OPENING CARDFILE', 'ERROR CLOSING CARDFILE', 'ABENDING PROGRAM', IO-STATUS-04
  (Lines: 71, 78, 85, 110, 129, 147, 155, 168, 172)

### Transformations
- **FD-CARDFILE-REC** → **CARD-RECORD**: Implicit move from file record to working storage record on READ
  (Lines: 93)
- **CARDFILE-STATUS** → **IO-STATUS**: MOVE CARDFILE-STATUS TO IO-STATUS for display formatting
  (Lines: 111, 130, 148)
- **IO-STATUS** → **IO-STATUS-04**: Format IO-STATUS into numeric display field IO-STATUS-04 handling non-numeric cases
  (Lines: 162, 164, 166, 170)

## Key Paragraphs

### MAINLINE
**Purpose:** Main control flow: display start, open file, loop read/display until EOF (PERFORM UNTIL END-OF-FILE = 'Y'; IF END-OF-FILE = 'N' PERFORM get-next and DISPLAY), close file, display end, GOBACK
- Calls: 0-CARDFILE-OPEN, 0-CARDFILE-GET-NEXT, 0-CARDFILE-CLOSE
- Lines: 70-87

### 0-CARDFILE-GET-NEXT
**Purpose:** Performs sequential READ of CARDFILE-FILE into CARD-RECORD, sets APPL-RESULT based on status ('00'=0, '10'=16, else=12), handles EOF and errors
- Called by: MAINLINE
- Calls: 0-DISPLAY-IO-STATUS, 9-ABEND-PROGRAM
- Lines: 92-115

### 0-CARDFILE-OPEN
**Purpose:** Opens CARDFILE-FILE for INPUT, sets APPL-RESULT (8 before, '00'=0 else=12), handles open errors
- Called by: MAINLINE
- Calls: 0-DISPLAY-IO-STATUS, 9-ABEND-PROGRAM
- Lines: 118-133

### 0-CARDFILE-CLOSE
**Purpose:** Closes CARDFILE-FILE, sets APPL-RESULT (+8 then adjust to 0 or 12 on status), handles close errors
- Called by: MAINLINE
- Calls: 0-DISPLAY-IO-STATUS, 9-ABEND-PROGRAM
- Lines: 136-151

### 9-ABEND-PROGRAM
**Purpose:** Displays abend message and calls CEE3ABD to terminate program abnormally (ABCODE=999, TIMING=0)
- Called by: 0-CARDFILE-GET-NEXT, 0-CARDFILE-OPEN, 0-CARDFILE-CLOSE
- Lines: 154-160

### 0-DISPLAY-IO-STATUS
**Purpose:** Formats and displays file IO-STATUS in IO-STATUS-04 for diagnostics (handles numeric/non-numeric)
- Called by: 0-CARDFILE-GET-NEXT, 0-CARDFILE-OPEN, 0-CARDFILE-CLOSE
- Lines: 161-173

## Error Handling

- **CARDFILE-STATUS NOT = '00' after READ and not '10':** DISPLAY 'ERROR READING CARDFILE', PERFORM 0-DISPLAY-IO-STATUS, PERFORM 9-ABEND-PROGRAM
  (Lines: 110, 111, 112, 113)
- **CARDFILE-STATUS NOT = '00' after OPEN:** DISPLAY 'ERROR OPENING CARDFILE', PERFORM 0-DISPLAY-IO-STATUS, PERFORM 9-ABEND-PROGRAM
  (Lines: 129, 130, 131, 132)
- **CARDFILE-STATUS NOT = '00' after CLOSE:** DISPLAY 'ERROR CLOSING CARDFILE', PERFORM 0-DISPLAY-IO-STATUS, PERFORM 9-ABEND-PROGRAM
  (Lines: 147, 148, 149, 150)
- **Program abend invoked:** DISPLAY 'ABENDING PROGRAM', CALL 'CEE3ABD' USING ABCODE=999, TIMING=0
  (Lines: 155, 156, 157, 158)

## Open Questions

- **What is the detailed structure and fields of CARD-RECORD defined in copybook CVACT02Y?**
  - Context: Referenced at line 45, used in READ INTO (93) and DISPLAY (78), but copybook content not provided
  - Suggestion: Obtain and analyze CVACT02Y copybook source
- **What is the exact function and parameters of CEE3ABD?**
  - Context: Called statically at line 158 with ABCODE and TIMING, appears to be abend routine but not defined in source
  - Suggestion: Consult mainframe Language Environment (LE) documentation or runtime library details

---
*Generated by War Rig WAR_RIG*