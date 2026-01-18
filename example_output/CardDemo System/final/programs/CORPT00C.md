# CORPT00C

**File:** CORPT00C.cbl
**Type:** COBOL
**Status:** FinalStatus.VALHALLA
**Iterations:** 2
**Analyzed:** 2026-01-18 16:53:20.674489

## Purpose

CICS online program that displays a screen for selecting transaction report types (Monthly, Yearly, Custom) and submits a batch job (TRNRPT00) via extra partition TDQ 'JOBS' containing JCL with date parameters. Validates user inputs including date fields using CSUTLDTC subroutine and numeric conversions. Handles screen navigation, error messages, and returns control to prior programs via XCTL.

**Business Context:** CardDemo application for printing transaction reports from online interface by submitting batch jobs to internal reader.
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 3, 5, 6, 24, 517, 549

## Calling Context

**Entry Points:** CR00
**Linkage Section:** DFHCOMMAREA, LK-COMMAREA

## Inputs

### CORPT0AI
- **Type:** CICS_MAP
- **Description:** Input map fields for report selection (MONTHLYI, YEARLYI, CUSTOMI), custom start/end dates (SDTMMI/SDTDDI/SDTYYYYI, EDTMMI/EDTDDI/EDTYYYYI), confirmation (CONFIRMI)
- **Copybook:** [CORPT00](../copybooks/CORPT00.md)
- **Lines:** 598, 599, 601

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Linkage commarea from calling program containing CARDDEMO-COMMAREA with prior context (CDEMO-PGM-REENTER, CDEMO-TO-PROGRAM)
- **Lines:** 155, 156, 176

### EIBAID
- **Type:** CICS_COMMAREA
- **Description:** Aid key from terminal (DFHENTER, DFHPF3) via DFHAID copybook
- **Copybook:** [DFHAID](../copybooks/DFHAID.md)
- **Lines:** 184, 148

## Outputs

### CORPT0AO
- **Type:** CICS_MAP
- **Description:** Output map for screen display including header info, error messages (ERRMSGO), titles
- **Copybook:** [CORPT00](../copybooks/CORPT00.md)
- **Lines:** 563, 571

### JOBS
- **Type:** CICS_QUEUE
- **Description:** Extra partition TDQ receiving JCL lines from JOB-DATA for batch job submission to internal reader
- **Lines:** 517, 518

### CARDDEMO-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated commarea returned to CICS or XCTL target
- **Lines:** 199, 587

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CSUTLDTC](./CSUTLDTC.md) | STATIC_CALL | Validate start date format conversion to YYYY-MM-DD | 392 |
| [CSUTLDTC](./CSUTLDTC.md) | STATIC_CALL | Validate end date format conversion to YYYY-MM-DD | 412 |
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | Transfer control to previous screen (e.g., COSGN00C or COMEN01C) | 549 |

## Business Rules

### BR001: For Monthly report, set start date to first day of current month and end date to last day of current month
**Logic:** Uses CURRENT-DATE, adjusts month/day for end date with DATE-OF-INTEGER
**Conditions:** MONTHLYI OF CORPT0AI NOT = SPACES AND LOW-VALUES
**Lines:** 213, 214, 217, 232

### BR002: For Yearly report, set dates to full current year (01/01 to 12/31)
**Logic:** Uses CURRENT-DATE year for both start/end
**Conditions:** YEARLYI OF CORPT0AI NOT = SPACES AND LOW-VALUES
**Lines:** 239, 243, 250

### BR003: Custom dates must be numeric, month 01-12, day 01-31, valid via CSUTLDTC; start/end not empty
**Logic:** NUMVAL-C conversion, range checks, CSUTLDTC call for validity
**Conditions:** SDTMMI/SDTDDI/SDTYYYYI/EDTMMI/EDTDDI/EDTYYYYI = SPACES OR LOW-VALUES, IS NOT NUMERIC OR SDTMMI/EDTMMI > '12', IS NOT NUMERIC OR SDTDDI/EDTDDI > '31', SDTYYYYI/EDTYYYYI IS NOT NUMERIC, CSUTLDTC-RESULT-SEV-CD NOT = '0000' AND MSG-NUM NOT = '2513'
**Lines:** 257, 305, 330, 338, 347, 355, 364, 373, 392, 412

### BR004: Confirmation must be 'Y'/'y' to submit job, 'N'/'n' to cancel, else invalid
**Logic:** EVALUATE CONFIRMI, prompt if empty
**Conditions:** CONFIRMI = SPACES OR LOW-VALUES, CONFIRMI = 'Y' OR 'y', CONFIRMI = 'N' OR 'n'
**Lines:** 464, 478, 481, 485

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CORPT00](../copybooks/CORPT00.md) | WORKING_STORAGE | Defines BMS map structures CORPT0AI (input) and CORPT0AO (output) for transaction report screen | 140 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | Defines EIBAID for aid key detection (ENTER, PF3) | 148 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | Defines BMS control areas for CICS map handling | 149 |
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Common definitions including CCDA-* constants for titles/messages | 138 |
| [CVTRA05Y](../copybooks/CVTRA05Y.md) | WORKING_STORAGE | Transaction data structures (WS-CURDATE-DATA referenced) | 146 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Common title definitions | 142 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Common date working storage definitions (e.g., WS-CURDATE-DATA) | 143 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Common message constants (e.g., CCDA-MSG-INVALID-KEY) | 144 |

## Data Flow

### Reads From
- **CORPT0AI**: MONTHLYI, YEARLYI, CUSTOMI, SDTMMI, SDTDDI, SDTYYYYI, EDTMMI, EDTDDI, EDTYYYYI, CONFIRMI
  (Lines: 212, 257, 464)
- **JOB-DATA**: JOB-LINES
  (Lines: 501)
- **EIBAID**: DFHENTER, DFHPF3
  (Lines: 184)

### Writes To
- **JOBS TDQ**: JCL-RECORD
  (Lines: 517)
- **CORPT0AO**: ERRMSGO, TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO
  (Lines: 560, 613)
- **JOB-DATA JCL symbolic parameters**: PARM-START-DATE-1, PARM-START-DATE-2, PARM-END-DATE-1, PARM-END-DATE-2
  (Lines: 221, 429)

### Transformations
- **SDTMMI/SDTDDI/SDTYYYYI** → **WS-START-DATE**: Convert to numeric via NUMVAL-C, assemble YYYY-MM-DD, validate with CSUTLDTC
  (Lines: 305, 381, 388)
- **EDTMMI/EDTDDI/EDTYYYYI** → **WS-END-DATE**: Convert to numeric via NUMVAL-C, assemble YYYY-MM-DD, validate with CSUTLDTC
  (Lines: 317, 384, 408)
- **JOB-LINES(WS-IDX)** → **JCL-RECORD**: Copy JCL line to record for TDQ write, loop until /*EOF
  (Lines: 501, 502)

## Key Paragraphs

### MAIN-PARA
**Purpose:** Main control: initialize flags, handle first entry/reentry, process aid keys, send/receive screens
- Calls: RETURN-TO-PREV-SCREEN, SEND-TRNRPT-SCREEN, RECEIVE-TRNRPT-SCREEN, PROCESS-ENTER-KEY
- Lines: 163-202

### PROCESS-ENTER-KEY
**Purpose:** Process report selection: set dates for monthly/yearly/custom, validate inputs, submit job if valid
- Called by: MAIN-PARA
- Calls: SUBMIT-JOB-TO-INTRDR, SEND-TRNRPT-SCREEN, INITIALIZE-ALL-FIELDS
- Lines: 208-456

### SUBMIT-JOB-TO-INTRDR
**Purpose:** Validate confirmation, loop to write JCL lines to TDQ 'JOBS'
- Called by: PROCESS-ENTER-KEY
- Calls: SEND-TRNRPT-SCREEN, INITIALIZE-ALL-FIELDS, WIRTE-JOBSUB-TDQ
- Lines: 462-510

### WIRTE-JOBSUB-TDQ
**Purpose:** Write single JCL record to TDQ with RESP check
- Called by: SUBMIT-JOB-TO-INTRDR
- Calls: SEND-TRNRPT-SCREEN
- Lines: 515-535

### SEND-TRNRPT-SCREEN
**Purpose:** Populate header, send map CORPT0A with optional ERASE
- Called by: MAIN-PARA, PROCESS-ENTER-KEY, SUBMIT-JOB-TO-INTRDR
- Calls: POPULATE-HEADER-INFO, RETURN-TO-CICS
- Lines: 556-578

### RECEIVE-TRNRPT-SCREEN
**Purpose:** Receive map into CORPT0AI with RESP
- Called by: MAIN-PARA
- Lines: 596-604

## Error Handling

- **Invalid aid key (OTHER than ENTER/PF3):** Set error flag, display invalid key message, resend screen
  (Lines: 191, 193)
- **CSUTLDTC-RESULT-SEV-CD NOT '0000' and MSG-NUM NOT '2513':** Display invalid date message, set error, resend screen
  (Lines: 396, 400, 416)
- **WRITEQ TD RESP NOT NORMAL:** Display error message, set error flag, resend screen
  (Lines: 525, 529)
- **Date field empty or invalid numeric/range:** Set specific error message, highlight field (MOVE -1 to L field), resend screen
  (Lines: 259, 330, 338)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| RETURN | TRANSID(WS-TRANID='CR00') COMMAREA(CARDDEMO-COMMAREA) | Return to CICS with transaction ID and commarea | 199 |
| XCTL | PROGRAM(CDEMO-TO-PROGRAM) | Transfer control to prior program (e.g., COSGN00C) | 549 |
| WRITEQ TD | QUEUE('JOBS') | Submit JCL to extra partition TDQ for job submission | 517 |
| SEND | MAP('CORPT0A') MAPSET('CORPT00') | Send screen with ERASE or CURSOR | 563 |
| RECEIVE | MAP('CORPT0A') MAPSET('CORPT00') | Receive user input from screen | 598 |

## Open Questions

- **Exact layout and validation details of CORPT0AI/O fields**
  - Context: Defined in unprovided CORPT00 copybook
  - Suggestion: Analyze CORPT00 copybook or BMS source
- **Structure of CARDDEMO-COMMAREA and prior calling programs**
  - Context: Passed via DFHCOMMAREA, fields like CDEMO-PGM-REENTER used but undefined here
  - Suggestion: Trace callers like COSGN00C or COMEN01C
- **Batch job PROC=TRANREPT details and output reports**
  - Context: JCL in JOB-DATA references AWS.M2.CARDDEMO.PROC(TRANREPT), processes TRANSACT file
  - Suggestion: Analyze PROC and related batch programs

---
*Generated by War Rig WAR_RIG*