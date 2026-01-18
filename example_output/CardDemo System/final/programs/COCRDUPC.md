# COCRDUPC

**File:** COCRDUPC.cbl
**Type:** COBOL
**Status:** FinalStatus.WITNESSED
**Iterations:** 1
**Analyzed:** 2026-01-18 15:44:26.861497

## Purpose

CICS online program for updating credit card details. Receives user input via map for account ID, card number, embossed name, active status, and expiry date; validates inputs; reads matching record from VSAM file CARDDAT using card number as key; populates screen with data; allows edits; confirms changes via PF5; rewrites record if confirmed and no conflicts.

**Business Context:** Credit card maintenance process: allows users to search by account/card, view/edit details (name, status, expiry), and commit updates to database.
**Program Type:** ONLINE_CICS
**Citations:** Lines 4, 24, 367, 492, 579, 1349, 1382, 1427, 1477

## Calling Context

**Entry Points:** CCUP
**Linkage Section:** DFHCOMMAREA

## Inputs

### CCRDUPAI
- **Type:** CICS_MAP
- **Description:** Screen input fields: ACCTSIDI (account ID), CARDSIDI (card number), CRDNAMEI (embossed name), CRDSTCDI (status), EXPMONI (expiry month), EXPYEARI (expiry year), EXPDAYI (expiry day)
- **Copybook:** [COCRDUP](../copybooks/COCRDUP.md)
- **Lines:** 579, 588, 598, 607, 614, 621, 630

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Program communication area containing CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA with prior context, old/new details, action flags
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 396, 397

### CARDDAT
- **Type:** FILE_VSAM
- **Description:** Credit card record read by card number key for display and update
- **Copybook:** [CVCRD01Y](../copybooks/CVCRD01Y.md)
- **Lines:** 1382, 1427

### DFHAID
- **Type:** CICS_COMMAREA
- **Description:** Attention identifier for PF keys and enter
- **Copybook:** [DFHAID](../copybooks/DFHAID.md)
- **Lines:** 414, 416, 418

## Outputs

### CCRDUPAO
- **Type:** CICS_MAP
- **Description:** Screen output fields populated with titles, dates, input echoes, error/info messages, data for edit
- **Copybook:** [COCRDUP](../copybooks/COCRDUP.md)
- **Lines:** 1329

### CARDDAT
- **Type:** FILE_VSAM
- **Description:** Rewritten credit card record with updated name, expiry date, status
- **Copybook:** [CVCRD01Y](../copybooks/CVCRD01Y.md)
- **Lines:** 1477

### WS-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Updated commarea with new action flags, old/new details for next invocation
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 549, 554

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | Exit to calling menu or list program on PF3 or after update | 473 |
| [COMEN01C](./COMEN01C.md) | CICS_XCTL | Default menu on exit if no from-program specified | 236 |
| [COCRDLIC](./COCRDLIC.md) | CICS_XCTL | Credit card list screen on exit after list-originated update | 227 |

## Business Rules

### BR001: Account ID must be 11-digit numeric non-zero if provided
**Logic:** Check NUMERIC, length implicit, >0; else error message
**Conditions:** CC-ACCT-ID NOT NUMERIC, CC-ACCT-ID-N = ZEROS
**Lines:** 740, 724, 753

### BR002: Card number must be 16-digit numeric non-zero if provided
**Logic:** Check NUMERIC, length implicit, >0; else error
**Conditions:** CC-CARD-NUM NOT NUMERIC, CC-CARD-NUM-N = ZEROS
**Lines:** 784, 768, 796

### BR003: Embossed name must be alphabetic only (A-Za-z spaces)
**Logic:** INSPECT CONVERTING non-alpha to spaces, then TRIM length=0 error
**Conditions:** INSPECT non-alpha, LENGTH(TRIM)=0
**Lines:** 824, 828

### BR004: Card status must be 'Y' or 'N'
**Logic:** Check against VALUES 'Y','N'
**Conditions:** NOT FLG-YES-NO-VALID
**Lines:** 861, 863

### BR005: Expiry month 01-12
**Logic:** 88 VALID-MONTH 1 THRU 12
**Conditions:** NOT VALID-MONTH
**Lines:** 895, 898

### BR006: Expiry year 1950-2099
**Logic:** 88 VALID-YEAR 1950 THRU 2099
**Conditions:** NOT VALID-YEAR
**Lines:** 932, 934

### BR007: No changes if new=old (uppercased); prompt no save
**Logic:** UPPER-CASE compare carDDATA
**Conditions:** UPPER(new)=UPPER(old)
**Lines:** 680

### BR008: Check for concurrent change before rewrite
**Logic:** Compare fetched CARD- fields to CCUP-OLD- after relock
**Conditions:** Fields differ
**Lines:** 1499, 1503

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVCRD01Y](../copybooks/CVCRD01Y.md) | WORKING_STORAGE | Defines CARD-RECORD layout for VSAM CARDDAT file | 268 |
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | Application commarea CARDDEMO-COMMAREA with context, from/to programs, acct/card keys | 272 |
| [COCRDUP](../copybooks/COCRDUP.md) | WORKING_STORAGE | BMS map CCRDUPA/AI/AO for screen input/output fields | 334 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | CICS BMS symbolic map constants (DFHBMPRF etc.) | 327 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | CICS AID keys (CCARD-AID-*) | 328 |
| [CVACT02Y](../copybooks/CVACT02Y.md) | WORKING_STORAGE | Card record layout (commented CVACT02Y line 353, but uses CARD-RECORD from CVCRD01Y) | 353 |

## Data Flow

### Reads From
- **CCRDUPAI**: ACCTSIDI, CARDSIDI, CRDNAMEI, CRDSTCDI, EXPMONI, EXPYEARI
  (Lines: 594, 603, 611, 618, 627, 634)
- **CARDDAT**: CARD-EMBOSSED-NAME, CARD-EXPIRAION-DATE, CARD-ACTIVE-STATUS, CARD-CVV-CD
  (Lines: 1354, 1361, 1367)

### Writes To
- **CARDDAT**: CARD-UPDATE-EMBOSSED-NAME, CARD-UPDATE-EXPIRAION-DATE, CARD-UPDATE-ACTIVE-STATUS
  (Lines: 1466, 1473, 1475)
- **CCRDUPAO**: ACCTSIDO, CARDSIDO, CRDNAMEO, CRDSTCDO, EXPMONO, EXPYEARO, ERRMSGO, INFOMSGO
  (Lines: 1090, 1096, 1108, 1161, 1163)

### Transformations
- **CCUP-NEW-CRDNAME** → **CARD-UPDATE-EMBOSSED-NAME**: Direct move after alpha validation
  (Lines: 1466)
- **CCUP-NEW-EXPYEAR,EXPMON,EXPDAY** → **CARD-UPDATE-EXPIRAION-DATE**: STRING year-'mon-'day DELIMITED BY SIZE
  (Lines: 1467)
- **CARD-EMBOSSED-NAME** → **CCUP-OLD-CRDNAME**: INSPECT upper case then substring move
  (Lines: 1356, 1360)

## Key Paragraphs

### 0000-MAIN
**Purpose:** Main control: handle abend, init vars/commarea, store PFkey, validate AID, evaluate action and PERFORM subflows
- Calls: YYYY-STORE-PFKEY, 9000-READ-DATA, 3000-SEND-MAP, 1000-PROCESS-INPUTS, 2000-DECIDE-ACTION
- Lines: 367-545

### 1000-PROCESS-INPUTS
**Purpose:** Receive map, edit inputs, set next-map/prog/error msg
- Called by: 0000-MAIN
- Calls: 1100-RECEIVE-MAP, 1200-EDIT-MAP-INPUTS
- Lines: 564-574

### 2000-DECIDE-ACTION
**Purpose:** Branch on flags/AID: read data, process write, handle errors/exit
- Called by: 0000-MAIN
- Calls: 9000-READ-DATA, 9200-WRITE-PROCESSING, ABEND-ROUTINE
- Lines: 948-1028

### 3000-SEND-MAP
**Purpose:** Build and send screen map with init, vars, info, attrs
- Called by: 0000-MAIN
- Calls: 3100-SCREEN-INIT, 3200-SETUP-SCREEN-VARS, 3250-SETUP-INFOMSG, 3300-SETUP-SCREEN-ATTRS, 3400-SEND-SCREEN
- Lines: 1035-1047

### 9000-READ-DATA
**Purpose:** Init old details, read VSAM by acct/card
- Called by: 0000-MAIN, 2000-DECIDE-ACTION
- Calls: 9100-GETCARD-BYACCTCARD
- Lines: 1343-1371

### 9200-WRITE-PROCESSING
**Purpose:** Read-update lock VSAM, check changes, build update rec, REWRITE
- Called by: 2000-DECIDE-ACTION
- Calls: 9300-CHECK-CHANGE-IN-REC
- Lines: 1420-1493

### ABEND-ROUTINE
**Purpose:** Send abend message, cancel handle, ABEND 9999
- Called by: 0000-MAIN
- Lines: 1531-1553

## Error Handling

- **Invalid AID/PFkey:** Set to ENTER, reposition cursor
  (Lines: 413, 423)
- **FILE READ RESP NOT NORMAL:** Set error flags/messages: NOTFND=combo not found, else file error msg
  (Lines: 1392)
- **UPDATE READ RESP NOT NORMAL:** Could not lock, error msg
  (Lines: 1441)
- **REWRITE RESP NOT NORMAL:** Set update failed flag/msg
  (Lines: 1488)
- **Any abend:** Send ABEND-DATA mapless, ABEND 9999
  (Lines: 370, 1539, 1550)
- **Data changed since fetch:** Refresh old details, show again
  (Lines: 1453, 1511)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| HANDLE ABEND | ABEND-ROUTINE | Catch unhandled exceptions to abend routine | 370 |
| RECEIVE MAP | CCRDUPA | Receive screen input into CCRDUPAI | 579 |
| SEND MAP | CCRDUPA | Send screen output from CCRDUPAO with cursor/erase/freekb | 1329 |
| READ FILE | CARDDAT | Read card record by cardnum RIDFLD | 1382 |
| READ FILE UPDATE | CARDDAT | Lock card record for rewrite | 1427 |
| REWRITE FILE | CARDDAT | Update card record | 1478 |
| XCTL PROGRAM | CDEMO-TO-PROGRAM | Transfer control to menu/list on exit | 473 |
| RETURN TRANSID | CCUP | Return to CICS with updated commarea for re-entry | 554 |

## Open Questions

- **Exact VSAM key structure for CARDDAT**
  - Context: Code sets WS-CARD-RID-ACCT-ID but READ RIDFLD only CARDNUM (16 bytes), KEYLENGTH=16; assumes cardnum unique despite acct context?
  - Suggestion: Review file definition or DDNAME clustering
- **Full called_by programs**
  - Context: XCTL to dynamic CDEMO-TO-PROGRAM (menu COCRDLIC etc.); no static callers visible
  - Suggestion: Trace commarea from callers like COMEN01C
- **CVV update handling**
  - Context: Sets CARD-UPDATE-CVV-CD from input? But input map lacks CVV field; copies old CVV
  - Suggestion: Confirm if CVV editable (code: MOVE CCUP-NEW-CVV-CD but new lacks CVV input)

---
*Generated by War Rig WAR_RIG*