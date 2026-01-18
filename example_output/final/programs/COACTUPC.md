# COACTUPC

**File:** COACTUPC.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 1
**Analyzed:** 2026-01-18 15:46:52.022810

## Purpose

CICS online transaction program that displays a screen for entering or updating account and associated customer details, validates extensive input fields including numerics, alphas, dates, US phone, SSN, FICO score, state/zip combinations, fetches existing data from VSAM files (card xref, account master, customer master), compares old vs new values, and performs REWRITE updates upon PF5 confirmation if no concurrent changes detected.

**Business Context:** Credit card account maintenance allowing updates to account limits, status, dates, balances, cycles, and linked customer personal data (names, address, phone, SSN, DOB, FICO, etc.)
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 4, 23, 859, 1040, 2575, 3608, 3888

## Calling Context

**Entry Points:** CAUP
**Linkage Section:** DFHCOMMAREA

## Inputs

### CACTUPAI
- **Type:** CICS_MAP
- **Description:** Input map fields for account ID, status, limits, dates, customer names, address, phones, SSN parts, DOB, FICO, etc.
- **Copybook:** [COACTUP](../copybooks/COACTUP.md)
- **Lines:** 1040, 1051, 1065, 1073, 1144, 1211, 1224, 1233, 1256, 1279, 1288, 1315, 1357, 1401, 1419

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Program communication area containing CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA with prior state like ACUP-OLD-DETAILS and ACUP-NEW-DETAILS
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 880, 889, 891

### CXACAIX
- **Type:** FILE_VSAM
- **Description:** Card account cross-reference file, read by account ID to get customer ID and card number
- **Copybook:** [CVACT03Y](../copybooks/CVACT03Y.md)
- **Lines:** 3654

### ACCTDAT
- **Type:** FILE_VSAM
- **Description:** Account master file, read by account ID for account details like status, balances, limits, dates
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 3703, 3894

### CUSTDAT
- **Type:** FILE_VSAM
- **Description:** Customer master file, read by customer ID for personal details like name, address, phone, SSN, DOB, FICO
- **Copybook:** [CVCUS01Y](../copybooks/CVCUS01Y.md)
- **Lines:** 3753, 3921

### DFHAID
- **Type:** PARAMETER
- **Description:** CICS AID for PF keys like PF3=exit, PF5=confirm, PF12=cancel
- **Copybook:** [DFHAID](../copybooks/DFHAID.md)
- **Lines:** 898, 905

## Outputs

### CACTUPAO
- **Type:** CICS_MAP
- **Description:** Output map populated with titles, dates, account/customer data (old or new), info/error messages, attributes for protect/color/cursor
- **Copybook:** [COACTUP](../copybooks/COACTUP.md)
- **Lines:** 3594

### ACCTDAT
- **Type:** FILE_VSAM
- **Description:** Rewritten account record with updated status, balances, limits, dates, group
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 4066

### CUSTDAT
- **Type:** FILE_VSAM
- **Description:** Rewritten customer record with updated names, address, phones, SSN, DOB, FICO, EFT, primary holder
- **Copybook:** [CVCUS01Y](../copybooks/CVCUS01Y.md)
- **Lines:** 4086

### WS-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated commarea with CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA for state persistence across pseudo-conversations
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 1010, 1015

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | Exit to calling program or menu on PF3 | 956 |

## Business Rules

### BR001: Account ID must be exactly 11-digit non-zero numeric
**Logic:** TEST-NUMVAL-C and check != ZEROS after move to numeric
**Conditions:** CC-ACCT-ID IS NOT NUMERIC, CC-ACCT-ID-N EQUAL ZEROS
**Lines:** 1802, 1803

### BR002: Names (first, last) must contain only alphabets and spaces, required except middle optional
**Logic:** INSPECT CONVERTING alpha chars to spaces, check TRIM length ==0 after
**Conditions:** FLG-ALPHA-NOT-OK, FLG-ALPHA-BLANK
**Lines:** 1925, 1930, 1563, 1579

### BR003: US Phone: 3-digit area (valid NA code, non-zero), 3-digit prefix (non-zero), 4-digit line (non-zero), optional overall
**Logic:** Separate NUMERIC checks per part, lookup VALID-GENERAL-PURP-CODE for area
**Conditions:** NOT NUMERIC, ==0, NOT VALID-GENERAL-PURP-CODE
**Lines:** 2246, 2316, 2370, 2296

### BR004: SSN part1 (3dig): not 000/666/900-999; parts 2/3 numeric non-blank
**Logic:** NUM-REQD edit + INVALID-SSN-PART1 condition values
**Conditions:** INVALID-SSN-PART1, FLG-EDIT-US-SSN-PART1-NOT-OK
**Lines:** 121, 2449, 2442

### BR005: FICO score 300-850
**Logic:** NUM-REQD (3dig) then FICO-RANGE-IS-VALID 300 THRU 850
**Conditions:** NOT FICO-RANGE-IS-VALID
**Lines:** 848, 2515

### BR006: State code valid US 2-letter, zip first2 matches state
**Logic:** VALID-US-STATE-CODE lookup + VALID-US-STATE-ZIP-CD2-COMBO
**Conditions:** NOT VALID-US-STATE-CODE, NOT VALID-US-STATE-ZIP-CD2-COMBO
**Lines:** 2494, 2537

### BR007: Signed numerics (limits, balances) valid NUMVAL-C
**Logic:** TEST-NUMVAL-C ==0
**Conditions:** TEST-NUMVAL-C !=0
**Lines:** 2201

### BR008: Dates valid CCYY-MM-DD format via CSUTLDWY copy
**Logic:** EDIT-DATE-CCYYMMDD perform
**Conditions:** WS-EDIT-DATE-FLGS invalid
**Lines:** 1480, 166

### BR009: No changes: exact field match old=new (case-insensitive strings, exact numerics/dates)
**Logic:** 1205-COMPARE-OLD-NEW full field-by-field EVALUATE
**Conditions:** ANY field differs
**Lines:** 1684

### BR010: Optimistic locking: re-read for UPDATE, compare fetched vs stored old values before REWRITE
**Logic:** 9700-CHECK-CHANGE-IN-REC field compares post READ UPDATE
**Conditions:** DATA-WAS-CHANGED-BEFORE-UPDATE
**Lines:** 3894, 3921, 3947

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [COACTUP](../copybooks/COACTUP.md) | WORKING_STORAGE | Defines input/output map CACTUPAI/CACTUPAO for account update screen fields | 623 |
| [CVACT01Y](../copybooks/CVACT01Y.md) | WORKING_STORAGE | Account master record layout ACCOUNT-RECORD | 640 |
| [CVCUS01Y](../copybooks/CVCUS01Y.md) | WORKING_STORAGE | Customer master record layout CUSTOMER-RECORD | 646 |
| [CVACT03Y](../copybooks/CVACT03Y.md) | WORKING_STORAGE | Card xref record layout CARD-XREF-RECORD | 643 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | CICS BMS map attribute constants like DFHBMFSE, DFHBMPRF | 615 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | CICS AID constants like CCARD-AID-PFK03 | 616 |
| [CSLKPCDY](../copybooks/CSLKPCDY.md) | WORKING_STORAGE | North America phone area code lookups VALID-GENERAL-PURP-CODE | 602 |
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Common commarea CARDDEMO-COMMAREA | 650 |
| [CSUTLDPY](../copybooks/CSUTLDPY.md) | WORKING_STORAGE | Date edit routines EDIT-DATE-CCYYMMDD | 4232 |
| [CSSETATY](../copybooks/CSSETATY.md) | WORKING_STORAGE | Attribute setting logic for screen fields via replacing | 3208 |

## Data Flow

### Reads From
- **CXACAIX**: XREF-CUST-ID, XREF-CARD-NUM
  (Lines: 3654)
- **ACCTDAT**: ACCT-ID, ACCT-ACTIVE-STATUS, ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, ACCT-OPEN-DATE, ACCT-EXPIRAION-DATE, ACCT-REISSUE-DATE, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT, ACCT-GROUP-ID
  (Lines: 3703)
- **CUSTDAT**: CUST-ID, CUST-FIRST-NAME, CUST-MIDDLE-NAME, CUST-LAST-NAME, CUST-ADDR-LINE-1, CUST-ADDR-LINE-2, CUST-ADDR-LINE-3, CUST-ADDR-STATE-CD, CUST-ADDR-ZIP, CUST-PHONE-NUM-1, CUST-PHONE-NUM-2, CUST-SSN, CUST-DOB-YYYY-MM-DD, CUST-FICO-CREDIT-SCORE
  (Lines: 3753)
- **CACTUPAI map**: ACCTSIDI, ACSTTUSI, ACRDLIMI, ACSHLIMI, ACURBALI, ACRCYCRI, ACRCYDBI, OPNYEARI, EXPYEARI, RISYEARI, ACTSSN1I, ACSFNAMI, ACSLNAMI, ACSADL1I, ACSSTTEI, ACSZIPCI, ACSPH1AI
  (Lines: 1040)

### Writes To
- **ACCTDAT**: ACCT-UPDATE-ID, ACCT-UPDATE-ACTIVE-STATUS, ACCT-UPDATE-CURR-BAL, ACCT-UPDATE-CREDIT-LIMIT, ACCT-UPDATE-CASH-CREDIT-LIMIT, ACCT-UPDATE-OPEN-DATE, ACCT-UPDATE-EXPIRAION-DATE, ACCT-UPDATE-REISSUE-DATE, ACCT-UPDATE-CURR-CYC-CREDIT, ACCT-UPDATE-CURR-CYC-DEBIT, ACCT-UPDATE-GROUP-ID
  (Lines: 4066)
- **CUSTDAT**: CUST-UPDATE-FIRST-NAME, CUST-UPDATE-MIDDLE-NAME, CUST-UPDATE-LAST-NAME, CUST-UPDATE-ADDR-LINE-1, CUST-UPDATE-ADDR-LINE-2, CUST-UPDATE-ADDR-LINE-3, CUST-UPDATE-ADDR-STATE-CD, CUST-UPDATE-ADDR-ZIP, CUST-UPDATE-PHONE-NUM-1, CUST-UPDATE-PHONE-NUM-2, CUST-UPDATE-SSN, CUST-UPDATE-DOB-YYYY-MM-DD, CUST-UPDATE-FICO-CREDIT-SCORE
  (Lines: 4086)
- **CACTUPAO map**: ACCTSIDO, ACSTTUSO, ACRDLIMO, ACURBALO, INFOMSGO, ERRMSGO
  (Lines: 3594)

### Transformations
- **ACUP-NEW-OPEN-DATE (parts)** → **ACCT-UPDATE-OPEN-DATE**: STRING year-'--'mon-'--'day DELIMITED BY SIZE
  (Lines: 3976)
- **Map phone parts ACSPH1AI/BI/CI** → **ACUP-NEW-CUST-PHONE-NUM-1**: Direct move to redefines parts, later STRING to '(###)###-####'
  (Lines: 1357, 4033)
- **Fetched ACCOUNT-RECORD dates** → **ACUP-OLD-*-YEAR/MON/DAY**: Substring moves: (1:4) year, (6:2) mon, (9:2) day
  (Lines: 3832)
- **ACUP-NEW-* numerics X to COMP** → **ACUP-NEW-*-N**: NUMVAL-C if TEST-NUMVAL-C==0
  (Lines: 1079)

## Key Paragraphs

### 0000-MAIN
**Purpose:** Main control: handle abend, init, process PFkey, decide action based on state/ACUP-CHANGE-ACTION/PFkeys
- Calls: YYYY-STORE-PFKEY, 1000-PROCESS-INPUTS, 2000-DECIDE-ACTION, 3000-SEND-MAP
- Lines: 859-1006

### COMMON-RETURN
**Purpose:** Set error msg, build/return commarea with transid CAUP
- Lines: 1007-1020

### 1000-PROCESS-INPUTS
**Purpose:** Receive map into CACTUPAI, move to ACUP-NEW-DETAILS, edit all fields
- Called by: 0000-MAIN
- Calls: 1100-RECEIVE-MAP, 1200-EDIT-MAP-INPUTS
- Lines: 1025-1035

### 1100-RECEIVE-MAP
**Purpose:** CICS RECEIVE MAP into CACTUPAI, move all I-fields to ACUP-NEW_* handling * blanks as LOW-VALUES
- Called by: 1000-PROCESS-INPUTS
- Lines: 1039-1425

### 1200-EDIT-MAP-INPUTS
**Purpose:** Edit search keys if not fetched, compare old/new, invoke field-specific edits (mandatory, alpha, num, phone, SSN, etc.), set INPUT-ERROR if any fail
- Called by: 1000-PROCESS-INPUTS
- Calls: 1210-EDIT-ACCOUNT, 1205-COMPARE-OLD-NEW, 1220-EDIT-YESNO, EDIT-DATE-CCYYMMDD, 1250-EDIT-SIGNED-9V2, 1265-EDIT-US-SSN, 1245-EDIT-NUM-REQD, 1225-EDIT-ALPHA-REQD, 1270-EDIT-US-STATE-CD
- Lines: 1429-1677

### 2000-DECIDE-ACTION
**Purpose:** EVALUATE state/PFkey: fetch data, confirm changes, write, handle errors/abend
- Called by: 0000-MAIN
- Calls: 9000-READ-ACCT, 9600-WRITE-PROCESSING, ABEND-ROUTINE
- Lines: 2562-2642

### 3000-SEND-MAP
**Purpose:** Init screen, setup vars/old/new data, info msg, attributes/protect/unprotect/cursor/color, CICS SEND MAP
- Called by: 0000-MAIN
- Calls: 3100-SCREEN-INIT, 3200-SETUP-SCREEN-VARS, 3250-SETUP-INFOMSG, 3300-SETUP-SCREEN-ATTRS, 3400-SEND-SCREEN
- Lines: 2649-2663

### 9000-READ-ACCT
**Purpose:** Chain read xref->acct->cust files into records, store to ACUP-OLD-DETAILS/CDEMO-*
- Called by: 2000-DECIDE-ACTION
- Calls: 9200-GETCARDXREF-BYACCT, 9300-GETACCTDATA-BYACCT, 9400-GETCUSTDATA-BYCUST, 9500-STORE-FETCHED-DATA
- Lines: 3608-3646

### 9600-WRITE-PROCESSING
**Purpose:** READ UPDATE acct/cust, check no changes since fetch, build/populate update records from ACUP-NEW_*, REWRITE both, SYNCPOINT ROLLBACK if fail
- Called by: 2000-DECIDE-ACTION
- Calls: 9700-CHECK-CHANGE-IN-REC
- Lines: 3888-4104

### ABEND-ROUTINE
**Purpose:** Send abend data screen, CANCEL HANDLE, ABEND ABCODE 9999
- Called by: 0000-MAIN
- Lines: 4203-4225

## Error Handling

- **CICS RESP != NORMAL after READ/REWRITE:** Set error flags/messages like DID-NOT-FIND-*, COULD-NOT-LOCK-*, update failed; ROLLBACK if REWRITE fail
  (Lines: 3664, 3713, 3907, 4076, 4095)
- **HANDLE ABEND:** Branch to ABEND-ROUTINE
  (Lines: 862)
- **Input edit fails (blank/invalid):** Set INPUT-ERROR, WS-RETURN-MSG specific like 'must be supplied', cursor to field, reprotect
  (Lines: 1431, 1671)
- **Optimistic lock fail (data changed):** DATA-WAS-CHANGED-BEFORE-UPDATE msg, refresh screen
  (Lines: 3947)
- **PFK-INVALID:** Force ENTER
  (Lines: 905)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| HANDLE ABEND | ABEND-ROUTINE | Catch unhandled errors/abends | 862 |
| RECEIVE MAP | LIT-THISMAP (CACTUPA) | Receive user input into CACTUPAI | 1040 |
| SEND MAP | CCARD-NEXT-MAP (CACTUPA) | Send output screen CACTUPAO with cursor/erase/FREEKB | 3594 |
| READ DATASET | CXACAIX/ACCTDAT/CUSTDAT | Fetch records by RIDFLD account/cust ID | 3654 |
| READ FILE UPDATE | ACCTDAT/CUSTDAT | Lock records for REWRITE | 3894 |
| REWRITE FILE | ACCTDAT/CUSTDAT | Commit updates | 4066 |
| RETURN TRANSID | LIT-THISTRANID (CAUP) | Pseudo-conversational return with commarea | 1015 |
| XCTL PROGRAM | CDEMO-TO-PROGRAM | Exit on PF3 to menu/caller | 956 |
| SYNCPOINT |  | Commit before XCTL | 953 |
| SYNCPOINT ROLLBACK |  | Rollback on update fail | 4100 |

## Open Questions

- **Exact calling programs/transactions**
  - Context: XCTL to CDEMO-TO-PROGRAM (set from prior commarea or menu), but no static callers in code
  - Suggestion: Trace CARDDEMO-COMMAREA usage in other progs
- **Full copybook layouts for data fields**
  - Context: Copies like CVACT01Y define records, but contents inferred from moves; e.g. ACCT-OPEN-DATE PIC X(10)
  - Suggestion: Analyze referenced copybooks
- **VSAM file organizations/details**
  - Context: READ DATASET RIDFLD, alternate path CXACAIX for acct index
  - Suggestion: JCL/IDCAMS for file defs

---
*Generated by War Rig WAR_RIG*