# PAUDBLOD

**File:** cbl/PAUDBLOD.CBL
**Type:** COBOL
**Status:** In Progress
**Iterations:** 1
**Analyzed:** 2026-01-30 19:45:40.293787

## Purpose

PAUDBLOD is an IMS database loader utility that sequentially reads root segment records from INFILE1 and inserts them as PAUTSUM0 segments into the IMS database using unqualified ISRT calls. It then reads child segment records from INFILE2, which include a ROOT-SEG-KEY, uses a qualified GU call to position on the corresponding root segment, and inserts the child as PAUTDTL1 segments using unqualified ISRT. Duplicate segments (status 'II') are skipped without error, while other IMS errors or file I/O failures cause an ABEND with return code 16.

**Business Context:** Supports loading pending authorization summary and detail data into an IMS hierarchical database for authorization processing, likely part of a financial or payment system given segment names like PAUTSUM0 (Pending Auth Summary) and PAUTDTL1 (Pending Auth Details).
**Program Type:** BATCH
**Citations:** Lines 18, 26, 44, 45, 169, 175, 177, 180, 244, 296, 321

## Calling Context

**Entry Points:** DLITCBL
**Linkage Section:** PAUTBPCB

## Inputs

### INFILE1
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file containing fixed-length 100-byte root segment records for PAUTSUM0 (Pending Authorization Summary) segments.
- **Lines:** 226

### INFILE2
- **Type:** FILE_SEQUENTIAL
- **Description:** Sequential file containing child segment records for PAUTDTL1 (Pending Authorization Details), prefixed with 11-digit COMP-3 ROOT-SEG-KEY matching root ACCNTID.
- **Lines:** 273

### PAUTBPCB
- **Type:** IMS_SEGMENT
- **Description:** IMS PCB mask providing database status, segment name, key feedback, and positioning for PAUT database access.
- **Copybook:** [PAUTBPCB](../copybooks/PAUTBPCB.cpy.md)
- **Lines:** 161, 165

## Outputs

### PAUTSUM0
- **Type:** IMS_SEGMENT
- **Description:** Root segments (Pending Authorization Summary) inserted into IMS database.
- **Copybook:** [CIPAUSMY](../copybooks/CIPAUSMY.cpy.md)
- **Lines:** 244

### PAUTDTL1
- **Type:** IMS_SEGMENT
- **Description:** Child segments (Pending Authorization Details) inserted under matching PAUTSUM0 root segments.
- **Copybook:** [CIPAUDTY](../copybooks/CIPAUDTY.cpy.md)
- **Lines:** 321

## Business Rules

### BR001: Skip insertion of root or child segments if already exists in database (IMS status 'II' indicates duplicate).
**Logic:** After ISRT call, check PAUT-PCB-STATUS for 'II' and display message but continue without abend.
**Conditions:** PAUT-PCB-STATUS = 'II'
**Lines:** 256, 329

### BR002: Only process child records with numeric ROOT-SEG-KEY to ensure valid qualification.
**Logic:** Before GU/ISRT for child, test IF ROOT-SEG-KEY IS NUMERIC.
**Conditions:** ROOT-SEG-KEY IS NUMERIC
**Lines:** 275

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [IMSFUNCS](../copybooks/IMSFUNCS.cpy.md) | WORKING_STORAGE | Defines IMS DL/I function codes such as FUNC-ISRT and FUNC-GU used in CBLTDLI calls. | 142 |
| [CIPAUSMY](../copybooks/CIPAUSMY.cpy.md) | WORKING_STORAGE | Defines layout of PENDING-AUTH-SUMMARY root segment (PAUTSUM0) for IMS database. | 149 |
| [CIPAUDTY](../copybooks/CIPAUDTY.cpy.md) | WORKING_STORAGE | Defines layout of PENDING-AUTH-DETAILS child segment (PAUTDTL1) for IMS database. | 153 |
| [PAUTBPCB](../copybooks/PAUTBPCB.cpy.md) | LINKAGE | Defines PCB mask structure for PAUT IMS database including status, segment name, and key feedback fields. | 161 |

## Data Flow

### Reads From
- **INFILE1**: INFIL1-REC (entire 100-byte record)
  (Lines: 229)
- **INFILE2**: ROOT-SEG-KEY, CHILD-SEG-REC (entire 200-byte record)
  (Lines: 277, 280)

### Writes To
- **PAUTSUM0**: PENDING-AUTH-SUMMARY (entire segment from CIPAUSMY)
  (Lines: 246)
- **PAUTDTL1**: PENDING-AUTH-DETAILS (entire segment from CIPAUDTY)
  (Lines: 323)

### Transformations
- **ROOT-SEG-KEY** → **QUAL-SSA-KEY-VALUE**: Direct move of 11-digit COMP-3 root key from child record to qualified SSA for GU positioning on parent root.
  (Lines: 277)
- **INFIL1-REC** → **PENDING-AUTH-SUMMARY**: Direct move of entire 100-byte input record to root segment working storage area.
  (Lines: 229)
- **CHILD-SEG-REC** → **PENDING-AUTH-DETAILS**: Direct move of entire 200-byte child record data to child segment working storage area.
  (Lines: 280)

## Key Paragraphs

### MAIN-PARA
**Purpose:** This is the primary entry point and orchestration paragraph that controls the entire program flow for IMS database loading. It begins with an alternate ENTRY 'DLITCBL' USING PAUTBPCB to support IMS batch invocation, followed by a display startup message. It consumes no direct inputs but relies on passed PCB in linkage. It then performs 1000-INITIALIZE to open input files and set dates. Next, it enters a loop performing 2000-READ-ROOT-SEG-FILE until END-ROOT-SEG-FILE='Y', processing all root records from INFILE1. After root loading completes, it loops on 3000-READ-CHILD-SEG-FILE until END-CHILD-SEG-FILE='Y' to load children from INFILE2. Finally, it performs 4000-FILE-CLOSE to shut down files and issues GOBACK. No explicit business decisions here beyond loop control via EOF flags; errors in subordinates propagate via ABEND. It calls no external programs but coordinates all internal paragraphs.
- Calls: 1000-INITIALIZE, 2000-READ-ROOT-SEG-FILE, 3000-READ-CHILD-SEG-FILE, 4000-FILE-CLOSE
- Lines: 169-187

### 1000-INITIALIZE
**Purpose:** This paragraph handles program startup initialization including date acceptance and input file opens. It consumes system date via ACCEPT FROM DATE/DAY into CURRENT-DATE and CURRENT-YYDDD, and displays them for logging. No business data inputs. It opens INFILE1 and INFILE2 as INPUT, checking WS-INFIL1-STATUS and WS-INFIL2-STATUS after each; if not spaces or '00', displays error and branches to 9999-ABEND. Outputs are open files ready for reads and initialized flags/variables implicitly via prior WORKING-STORAGE. No decisions beyond file status validation for successful open. Error handling is immediate ABEND on open failure. Calls no other paragraphs.
- Called by: MAIN-PARA
- Lines: 190-217

### 2000-READ-ROOT-SEG-FILE
**Purpose:** This paragraph implements the read loop for root segment input file, reading one record at a time from INFILE1. It consumes sequential records via READ INFILE1 into INFIL1-REC. If WS-INFIL1-STATUS is spaces or '00', moves entire record to PENDING-AUTH-SUMMARY and performs 2100-INSERT-ROOT-SEG to load into IMS. If status '10', sets END-ROOT-SEG-FILE='Y' to exit loop. Other statuses trigger error display but no abend here (handled in caller loop). Business logic is simple EOF/success/fail decision on file status. Produces loaded root segments via subordinate and increments counters implicitly if used. Error handling defers to insert paragraph. Called repeatedly by MAIN-PARA until EOF.
- Called by: MAIN-PARA
- Calls: 2100-INSERT-ROOT-SEG
- Lines: 222-239

### 2100-INSERT-ROOT-SEG
**Purpose:** This paragraph performs the IMS ISRT for root segments using unqualified SSA. It consumes PENDING-AUTH-SUMMARY (loaded from input record) and issues CALL 'CBLTDLI' USING FUNC-ISRT PAUTBPCB PENDING-AUTH-SUMMARY ROOT-UNQUAL-SSA. Displays decorative messages and checks PAUT-PCB-STATUS post-call: spaces indicates success, 'II' duplicate (logged), others cause display error and 9999-ABEND. Business logic enforces no duplicates via status check without delete/reinsert. Outputs updated IMS database with new/duplicate-skipped root. No field validations beyond IMS. Error handling abends on non-success/II. No subordinate calls.
- Called by: 2000-READ-ROOT-SEG-FILE
- Lines: 242-264

### 3000-READ-CHILD-SEG-FILE
**Purpose:** This paragraph reads child segment records from INFILE2 in a loop. Consumes records via READ INFILE2 into INFIL2-REC (ROOT-SEG-KEY + CHILD-SEG-REC). If status spaces/'00' and ROOT-SEG-KEY IS NUMERIC, moves key to QUAL-SSA-KEY-VALUE for qualification, moves CHILD-SEG-REC to PENDING-AUTH-DETAILS, and performs 3100-INSERT-CHILD-SEG. If '10', sets END-CHILD-SEG-FILE='Y'. Other statuses display error. Business logic validates key numeric to prevent invalid GU positioning. Produces positioned context for child insert via subordinate. Errors handled in insert.
- Called by: MAIN-PARA
- Calls: 3100-INSERT-CHILD-SEG
- Lines: 269-290

### 3100-INSERT-CHILD-SEG
**Purpose:** This paragraph positions on parent root via GU before child ISRT, ensuring parentage. Consumes ROOT-SEG-KEY in SSA and PENDING-AUTH-SUMMARY I/O area; initializes PAUT-PCB-STATUS, CALL 'CBLTDLI' FUNC-GU PAUTBPCB PENDING-AUTH-SUMMARY ROOT-QUAL-SSA. Displays messages; if status not spaces/'II', displays failure details (status, KEYFB) and ABENDs. On success, performs 3200-INSERT-IMS-CALL for child insert. Business logic verifies root existence before child insert to enforce hierarchy. Outputs positioned PCB for child insert. Errors abend on GU fail.
- Called by: 3000-READ-CHILD-SEG-FILE
- Calls: 3200-INSERT-IMS-CALL
- Lines: 292-315

### 3200-INSERT-IMS-CALL
**Purpose:** This paragraph executes the child segment ISRT after parent GU. Consumes PENDING-AUTH-DETAILS and CHILD-UNQUAL-SSA; CALL 'CBLTDLI' FUNC-ISRT PAUTBPCB PENDING-AUTH-DETAILS CHILD-UNQUAL-SSA. Checks PAUT-PCB-STATUS: spaces success, 'II' duplicate logged, others display status/KEYFB and ABEND. Business logic skips duplicates. Outputs child segment in IMS under positioned parent. No further validations.
- Called by: 3100-INSERT-CHILD-SEG
- Lines: 318-338

### 4000-FILE-CLOSE
**Purpose:** This cleanup paragraph closes input files post-processing. Consumes open INFILE1/INFILE2; issues CLOSE for each, checks status (spaces/'00' continue, else display error but no abend). No inputs/data consumed. Outputs closed files. No business logic or decisions beyond status log. No error abend, allows graceful exit.
- Called by: MAIN-PARA
- Lines: 341-357

### 9999-ABEND
**Purpose:** Universal error termination paragraph invoked on any fatal condition. Consumes no data; displays 'IMS LOAD ABENDING ...', sets RETURN-CODE=16, GOBACKs. Role is to halt execution with non-zero code. No inputs/outputs modified. No conditions checked here. Called from multiple error paths.
- Called by: 1000-INITIALIZE, 2100-INSERT-ROOT-SEG, 3100-INSERT-CHILD-SEG, 3200-INSERT-IMS-CALL
- Lines: 360-368

## Inter-Paragraph Data Flow

| Caller | Callee | Inputs | Outputs | Purpose |
|--------|--------|--------|---------|---------|
| MAIN-PARA | 1000-INITIALIZE | - | CURRENT-DATE, CURRENT-YYDDD, WS-INFIL1-STATUS, WS-INFIL2-STATUS | Initializes current date variables from the system and opens input files INFILE1 and INFILE2. |
| MAIN-PARA | 2000-READ-ROOT-SEG-FILE | - | PENDING-AUTH-SUMMARY, END-ROOT-SEG-FILE | Reads a root segment record from INFILE1, moves it to the pending summary area if successful, and sets the end-of-file flag if end of file is reached. |
| MAIN-PARA | 3000-READ-CHILD-SEG-FILE | ROOT-SEG-KEY | QUAL-SSA-KEY-VALUE, PENDING-AUTH-DETAILS, END-CHILD-SEG-FILE | Reads a child segment record from INFILE2, qualifies it with the root segment key if numeric, moves it to the pending details area if matching, and sets the end-of-file flag if end of file is reached. |
| MAIN-PARA | 4000-FILE-CLOSE | - | WS-INFIL1-STATUS, WS-INFIL2-STATUS | Closes input files INFILE1 and INFILE2. |
| 1000-INITIALIZE | 9999-ABEND | - | RETURN-CODE | Abends the program by displaying a message and setting the return code to 16. |
| 1000-INITIALIZE | 9999-ABEND | - | RETURN-CODE | Terminates the program with return code 16 due to INFILE1 open failure. |
| 2000-READ-ROOT-SEG-FILE | 2100-INSERT-ROOT-SEG | PAUTBPCB, PENDING-AUTH-SUMMARY, ROOT-UNQUAL-SSA | PAUT-PCB-STATUS | Inserts the root authorization summary segment into the IMS database. |
| 2100-INSERT-ROOT-SEG | 9999-ABEND | - | RETURN-CODE | Terminates the program with return code 16 due to root segment insert failure. |
| 3000-READ-CHILD-SEG-FILE | 3100-INSERT-CHILD-SEG | PAUTBPCB, PENDING-AUTH-SUMMARY, ROOT-QUAL-SSA | PAUT-PCB-STATUS | Positions to the root segment via IMS GU call to prepare for child segment insertion. |
| 3100-INSERT-CHILD-SEG | 3200-INSERT-IMS-CALL | PAUTBPCB, PENDING-AUTH-DETAILS, CHILD-UNQUAL-SSA | PAUT-PCB-STATUS | Inserts the child authorization details segment into the IMS database. |
| 3100-INSERT-CHILD-SEG | 9999-ABEND | - | RETURN-CODE | Invokes the abend routine to terminate the program with return code 16 when the IMS GU call for the root segment fails. |
| 3200-INSERT-IMS-CALL | 9999-ABEND | - | RETURN-CODE | Invokes the abend routine to terminate the program with return code 16 when the IMS ISRT call for the child segment fails. |

## Error Handling

- **WS-INFIL1-STATUS NOT = SPACES OR '00' on OPEN/READ:** DISPLAY error message and PERFORM 9999-ABEND
  (Lines: 202, 232)
- **WS-INFIL2-STATUS NOT = SPACES OR '00' on OPEN/READ:** DISPLAY error message and PERFORM 9999-ABEND
  (Lines: 210, 283)
- **PAUT-PCB-STATUS NOT = SPACES OR 'II' after ISRT/GU:** DISPLAY status/KEYFB and PERFORM 9999-ABEND
  (Lines: 259, 310, 332)

## Open Questions

- **Exact field layouts and key fields in CIPAUSMY and CIPAUDTY copybooks**
  - Context: Copybooks not provided; cannot detail specific data transformations or validations beyond record moves
  - Suggestion: Provide copybook source for field-level analysis
- **Purpose and usage of unused WS variables like PRM-INFO, WS-NO-CHKP, counters (WS-TOT-REC-WRITTEN etc.)**
  - Context: Defined but never referenced or incremented in visible code
  - Suggestion: Check for conditional compilation or prior versions
- **Relationship to 'IMSUNLOD' in WS-PGMNAME**
  - Context: Variable set to 'IMSUNLOD' but program is PAUDBLOD; possible rename or related utility
  - Suggestion: Review invocation JCL or prior documentation

## Sequence Diagram

```mermaid
sequenceDiagram
    MAIN-PARA->>1000-INITIALIZE: performs
    1000-INITIALIZE-->>MAIN-PARA: CURRENT-DATE, CURRENT-YYDDD, WS-INFIL1-STATUS...
    MAIN-PARA->>2000-READ-ROOT-SEG-FILE: performs
    2000-READ-ROOT-SEG-FILE-->>MAIN-PARA: PENDING-AUTH-SUMMARY, END-ROOT-SEG-FILE
    MAIN-PARA->>3000-READ-CHILD-SEG-FILE: ROOT-SEG-KEY
    3000-READ-CHILD-SEG-FILE-->>MAIN-PARA: QUAL-SSA-KEY-VALUE, PENDING-AUTH-DETAILS, END-CHILD-SEG-FILE
    MAIN-PARA->>4000-FILE-CLOSE: performs
    4000-FILE-CLOSE-->>MAIN-PARA: WS-INFIL1-STATUS, WS-INFIL2-STATUS
    1000-INITIALIZE->>9999-ABEND: performs
    9999-ABEND-->>1000-INITIALIZE: RETURN-CODE
    1000-INITIALIZE->>9999-ABEND: performs
    9999-ABEND-->>1000-INITIALIZE: RETURN-CODE
    2000-READ-ROOT-SEG-FILE->>2100-INSERT-ROOT-SEG: PAUTBPCB, PENDING-AUTH-SUMMARY, ROOT-UNQUAL-SSA
    2100-INSERT-ROOT-SEG-->>2000-READ-ROOT-SEG-FILE: PAUT-PCB-STATUS
    2100-INSERT-ROOT-SEG->>CBLTDLI: performs
    2100-INSERT-ROOT-SEG->>9999-ABEND: performs
    9999-ABEND-->>2100-INSERT-ROOT-SEG: RETURN-CODE
    3000-READ-CHILD-SEG-FILE->>3100-INSERT-CHILD-SEG: PAUTBPCB, PENDING-AUTH-SUMMARY, ROOT-QUAL-SSA
    3100-INSERT-CHILD-SEG-->>3000-READ-CHILD-SEG-FILE: PAUT-PCB-STATUS
    3100-INSERT-CHILD-SEG->>CBLTDLI: performs
    3100-INSERT-CHILD-SEG->>3200-INSERT-IMS-CALL: PAUTBPCB, PENDING-AUTH-DETAILS, CHILD-UNQUAL-SSA
    3200-INSERT-IMS-CALL-->>3100-INSERT-CHILD-SEG: PAUT-PCB-STATUS
    3100-INSERT-CHILD-SEG->>9999-ABEND: performs
    9999-ABEND-->>3100-INSERT-CHILD-SEG: RETURN-CODE
    3200-INSERT-IMS-CALL->>CBLTDLI: performs
    3200-INSERT-IMS-CALL->>9999-ABEND: performs
    9999-ABEND-->>3200-INSERT-IMS-CALL: RETURN-CODE
```

---
*Generated by War Rig WAR_RIG*