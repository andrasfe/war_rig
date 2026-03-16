# DBUNLDGS

**File:** cbl/DBUNLDGS.CBL
**Type:** COBOL
**Status:** In Progress
**Iterations:** 1
**Analyzed:** 2026-03-16 20:04:07.904356

## Purpose

The COBOL program DBUNLDGS extracts data from an IMS database related to pending authorizations and writes it to sequential output files. It reads pending authorization summary segments (root) and pending authorization details segments (child) from the IMS database and outputs them to OPFILE1 and OPFILE2 respectively.

**Business Context:** This program likely supports auditing or reporting on pending authorization requests within a financial or similar institution.
**Program Type:** BATCH
**Citations:** Lines 17, 147, 211

## Calling Context

**Entry Points:** DLITCBL
**Linkage Section:** PAUTBPCB, PASFLPCB, PADFLPCB

## Inputs

### IMS Database (PAUTSUM0, PAUTDTL1)
- **Type:** IMS_SEGMENT
- **Description:** Pending Authorization Summary (root segment) and Pending Authorization Details (child segment) from an IMS database.
- **Copybook:** [CIPAUSMY, CIPAUDTY](../copybooks/CIPAUSMY, CIPAUDTY.cpy.md)
- **Lines:** 224, 243, 269, 282

### SYSIN
- **Type:** PARAMETER
- **Description:** Input parameters for the program, specifically P-EXPIRY-DAYS, P-CHKP-FREQ, P-CHKP-DIS-FREQ, and P-DEBUG-FLAG, although the ACCEPT statement is commented out.
- **Lines:** 163

## Outputs

### OPFILE1
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file containing records extracted from the PENDING-AUTH-SUMMARY (root) segment of the IMS database.
- **Lines:** 211, 2171000

### OPFILE2
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file containing records extracted from the PENDING-AUTH-DETAILS (child) segment of the IMS database.
- **Lines:** 254, 2551000

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CBLTDLI](./CBLTDLI.cbl.md) | STATIC_CALL | Interface with IMS database to retrieve segments. | 197 |
| [CBLTDLI](./CBLTDLI.cbl.md) | STATIC_CALL | Interface with IMS database to retrieve segments. | 241 |

## Business Rules

### BR001: Only extract pending authorization details if the PA-ACCT-ID is numeric.
**Logic:** The program checks if PA-ACCT-ID is numeric before extracting child segments.
**Conditions:** IF PA-ACCT-ID IS NUMERIC
**Lines:** 216, 247

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [IMSFUNCS](../copybooks/IMSFUNCS.cpy.md) | WORKING_STORAGE | Contains definitions for IMS function codes used in CBLTDLI calls. | 116 |
| [CIPAUSMY](../copybooks/CIPAUSMY.cpy.md) | WORKING_STORAGE | Defines the layout of the PENDING-AUTH-SUMMARY segment (root segment). | 123 |
| [CIPAUDTY](../copybooks/CIPAUDTY.cpy.md) | WORKING_STORAGE | Defines the layout of the PENDING-AUTH-DETAILS segment (child segment). | 147 |
| [PAUTBPCB](../copybooks/PAUTBPCB.cpy.md) | LINKAGE | Defines the Program Communication Block (PCB) for the PAUTSUM0 database. | 134 |
| [PASFLPCB](../copybooks/PASFLPCB.cpy.md) | LINKAGE | Defines the Program Communication Block (PCB) for an alternate PCB. | 1341000 |
| [PADFLPCB](../copybooks/PADFLPCB.cpy.md) | LINKAGE | Defines the Program Communication Block (PCB) for an alternate PCB. | 1342000 |

## Data Flow

### Reads From
- **IMS Database (PENDING-AUTH-SUMMARY)**: PA-ACCT-ID
  (Lines: 214)
- **IMS Database (PENDING-AUTH-DETAILS)**: All fields in CIPAUDTY copybook
  (Lines: 254)

### Writes To
- **OPFILE1**: All fields in CIPAUSMY copybook
  (Lines: 211)
- **OPFILE2**: ROOT-SEG-KEY, CHILD-SEG-REC
  (Lines: 254)

## Key Paragraphs

### MAIN-PARA
**Purpose:** This is the main paragraph of the program, serving as the entry point and orchestrating the overall data extraction process. It first performs 1000-INITIALIZE to set up the program environment, including accepting the current date and displaying initial messages. Then, it enters a loop, controlled by the WS-END-OF-ROOT-SEG flag, that repeatedly calls 2000-FIND-NEXT-AUTH-SUMMARY to retrieve and process pending authorization summary segments from the IMS database. The 2000-FIND-NEXT-AUTH-SUMMARY paragraph retrieves the root segment and then calls 3000-FIND-NEXT-AUTH-DTL to retrieve the child segments. Once all summary segments have been processed (WS-END-OF-ROOT-SEG is set to 'Y'), the program performs 4000-FILE-CLOSE to close the output files. Finally, the program terminates with a GOBACK statement.
- Calls: 1000-INITIALIZE, 2000-FIND-NEXT-AUTH-SUMMARY, 4000-FILE-CLOSE
- Lines: 141-179

### 1000-INITIALIZE
**Purpose:** This paragraph initializes the program environment. It accepts the current date and day from the system. It then displays a starting message including the current date to the console. The paragraph contains commented-out code to accept parameters from SYSIN, which would include expiry days, checkpoint frequency, checkpoint display frequency, and a debug flag. It also contains commented-out code to open OPFILE1 and OPFILE2, checking their status and abending if an error occurs. The paragraph does not read any files or modify any data other than setting the current date variables. It does not call any other paragraphs or programs.
- Called by: MAIN-PARA
- Lines: 157-188

### 2000-FIND-NEXT-AUTH-SUMMARY
**Purpose:** This paragraph retrieves the next pending authorization summary segment (root segment) from the IMS database. It initializes the PAUT-PCB-STATUS and then calls the CBLTDLI routine with the FUNC-GN (Get Next) function code to retrieve the next summary segment. If the call is successful (PAUT-PCB-STATUS is spaces), it increments counters for summary records read and processed, moves the retrieved segment to OPFIL1-REC, and moves the PA-ACCT-ID to ROOT-SEG-KEY. It then checks if PA-ACCT-ID is numeric and, if so, calls 3100-INSERT-PARENT-SEG-GSAM to write the parent segment to GSAM and then calls 3000-FIND-NEXT-AUTH-DTL to retrieve the associated detail segments (child segments). The detail segment retrieval continues until WS-END-OF-CHILD-SEG is set to 'Y'. If the PAUT-PCB-STATUS is 'GB' (end of database), it sets the WS-END-OF-ROOT-SEG flag to 'Y'. If the PAUT-PCB-STATUS indicates an error, it displays an error message and abends the program.
- Called by: MAIN-PARA
- Calls: CBLTDLI, 3100-INSERT-PARENT-SEG-GSAM, 3000-FIND-NEXT-AUTH-DTL
- Lines: 191-231

### 3000-FIND-NEXT-AUTH-DTL
**Purpose:** This paragraph retrieves the next pending authorization detail segment (child segment) from the IMS database. It calls the CBLTDLI routine with the FUNC-GNP (Get Next within Parent) function code to retrieve the next detail segment associated with the current summary segment. If the call is successful (PAUT-PCB-STATUS is spaces), it sets the MORE-AUTHS flag to TRUE, increments counters, and moves the retrieved segment to CHILD-SEG-REC. It then calls 3200-INSERT-CHILD-SEG-GSAM to write the child segment to GSAM. If the PAUT-PCB-STATUS is 'GE' (end of parent), it sets the WS-END-OF-CHILD-SEG flag to 'Y'. If the PAUT-PCB-STATUS indicates an error, it displays an error message and abends the program. Finally, it initializes PAUT-PCB-STATUS.
- Called by: 2000-FIND-NEXT-AUTH-SUMMARY
- Calls: CBLTDLI, 3200-INSERT-CHILD-SEG-GSAM
- Lines: 237-268

### 4000-FILE-CLOSE
**Purpose:** This paragraph is responsible for closing the output files. However, the code to close the files is commented out. Therefore, this paragraph currently performs no action. It is called by MAIN-PARA after all the IMS segments have been processed. It does not read any input, modify any data, or call any other paragraphs or programs.
- Called by: MAIN-PARA
- Lines: 150-175

### 3100-INSERT-PARENT-SEG-GSAM
**Purpose:** This paragraph is intended to insert the parent segment into a GSAM dataset. However, the code is incomplete and only contains the paragraph declaration and a partial comment. It is called from 2000-FIND-NEXT-AUTH-SUMMARY after a parent segment has been successfully read from the IMS database and the PA-ACCT-ID is numeric. It is expected to write the OPFIL1-REC to a GSAM dataset, but the actual WRITE statement and file handling are missing.
- Called by: 2000-FIND-NEXT-AUTH-SUMMARY
- Lines: 2171000-301

### 3200-INSERT-CHILD-SEG-GSAM
**Purpose:** This paragraph is intended to insert the child segment into a GSAM dataset. However, the code is missing. It is called from 3000-FIND-NEXT-AUTH-DTL after a child segment has been successfully read from the IMS database. It is expected to write the OPFIL2-REC to a GSAM dataset, but the actual WRITE statement and file handling are missing.
- Called by: 3000-FIND-NEXT-AUTH-DTL

## Error Handling

- **PAUT-PCB-STATUS NOT EQUAL TO SPACES AND 'GB':** DISPLAY error message and ABEND
  (Lines: 227, 230)
- **PAUT-PCB-STATUS NOT EQUAL TO SPACES AND 'GE':** DISPLAY error message and ABEND
  (Lines: 290, 293)

## Open Questions

- **What are the exact layouts of the GSAM datasets that 3100-INSERT-PARENT-SEG-GSAM and 3200-INSERT-CHILD-SEG-GSAM are supposed to write to?**
  - Context: The WRITE statements and file definitions for the GSAM datasets are missing.
  - Suggestion: Examine related documentation or source code to determine the GSAM file layouts.

## Resolved Questions

- **Q:** Why are the OPEN and CLOSE statements for OPFILE1 and OPFILE2 commented out?
  **A:** The `SELECT`, `FD`, `OPEN`, and `CLOSE` statements for both `OPFILE1` and `OPFILE2` are commented out. This suggests that the program, in its current state, does not directly manage these files.

INCONCLUSIVE: Without further information, it's impossible to determine the exact reason why these statements are commented out. It could be due to several reasons:

1.  The file handling might be performed by a called subroutine or a different mechanism (e.g., system call).
2.  The program might be designed to be used in an environment where the files are pre-opened or handled by the operating system.
3.  The file output functionality might be disabled or under development.
4.  The program might be using an alternative method for writing to the output files.

To determine the actual reason, further investigation is needed to understand how the output files are handled, possibly by examining other parts of the code or related documentation.

---
*Generated by War Rig WAR_RIG*