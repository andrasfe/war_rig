# COACTVWC

**File:** COACTVWC.cbl
**Type:** COBOL
**Status:** FinalStatus.VALHALLA
**Iterations:** 2
**Analyzed:** 2026-01-18 16:07:43.482487

## Purpose

COACTVWC is a CICS online transaction (CAVW) that displays a screen prompting for an 11-digit account ID, receives and validates the input for numeric non-zero value, sequentially reads VSAM files CXACAIX (card xref by acct path) to get cust ID/card num, ACCTDAT (account master) for status/balances/dates, and CUSTDAT (customer master) for name/address/phone/SSN/FICO, then populates and redisplays the screen with retrieved data or error messages. On PF3, it XCTLs back to caller or main menu (COMEN01C). Errors like NOTFND or invalid input trigger reprompt with specific messages.

**Business Context:** Part of CardDemo application for viewing linked account/customer details via card cross-reference, supporting account inquiry in banking/card services.
**Program Type:** ONLINE_CICS
**Citations:** Lines 2, 4, 23, 146, 262, 358, 362, 596, 687, 693, 701, 710

## Calling Context

**Called By:** [COMEN01C](./COMEN01C.md)
**Entry Points:** CAVW
**Linkage Section:** DFHCOMMAREA

## Inputs

### CACTVWAI
- **Type:** CICS_MAP
- **Description:** Screen input map received via CICS RECEIVE MAP, key field ACCTSIDI (account ID filter, edited to CC-ACCT-ID replacing * or spaces with LOW-VALUES)
- **Copybook:** [COACTVW](../copybooks/COACTVW.md)
- **Lines:** 611, 613, 622, 628, 632

### DFHCOMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Incoming commarea parsed into CARDDEMO-COMMAREA (COCOM01Y: context like CDEMO-FROM-PROGRAM/TRANID) and WS-THIS-PROGCOMMAREA (CA-CALL-CONTEXT), used to persist CDEMO-ACCT-ID/CUST-ID/CARD-NUM across reenters
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 257, 282, 288, 289

### CXACAIX
- **Type:** FILE_VSAM
- **Description:** Card-account cross-reference VSAM file, read by alternate path ACCT-ID (WS-CARD-RID-ACCT-ID-X) to retrieve XREF-CUST-ID and XREF-CARD-NUM
- **Copybook:** [CVACT03Y](../copybooks/CVACT03Y.md)
- **Lines:** 727, 728, 739, 740

### ACCTDAT
- **Type:** FILE_VSAM
- **Description:** Account master VSAM file, read by ACCT-ID (WS-CARD-RID-ACCT-ID-X) to retrieve fields like ACCT-ACTIVE-STATUS, ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, dates
- **Copybook:** [CVACT01Y](../copybooks/CVACT01Y.md)
- **Lines:** 776, 777, 788

### CUSTDAT
- **Type:** FILE_VSAM
- **Description:** Customer master VSAM file, read by CUST-ID from xref (WS-CARD-RID-CUST-ID-X) to retrieve CUST-ID, SSN (formatted), FICO, DOB, names, address, phone, govt ID, EFT acct
- **Copybook:** [CVCUS01Y](../copybooks/CVCUS01Y.md)
- **Lines:** 826, 827, 838

## Outputs

### CACTVWAO
- **Type:** CICS_MAP
- **Description:** Output screen map sent via CICS SEND MAP (CCARD-NEXT-MAP=CACTVWA), populated with titles/dates/transaction/program (lines 436-453), input account ID (468), account details if found (471-491), customer details if found (494-523), error msg (532), info msg/prompt (533, 527), with attributes/color/cursor positioning (541-572)
- **Copybook:** [COACTVW](../copybooks/COACTVW.md)
- **Lines:** 432, 468, 471, 494, 532, 533, 541, 583

### WS-COMMAREA
- **Type:** CICS_COMMAREA
- **Description:** Outgoing commarea combining updated CARDDEMO-COMMAREA (with new CDEMO-ACCT-ID/CUST-ID/CARD-NUM, context like CDEMO-TO-PROGRAM/TRANID/LAST-MAPSET) and WS-THIS-PROGCOMMAREA, returned via CICS RETURN TRANSID(CAVW)
- **Copybook:** [COCOM01Y](../copybooks/COCOM01Y.md)
- **Lines:** 397, 398, 402

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CDEMO-TO-PROGRAM](./CDEMO-TO-PROGRAM.md) | CICS_XCTL | On PF3=ENTER, XCTL to original caller (CDEMO-FROM-PROGRAM) or default menu COMEN01C, passing updated CARDDEMO-COMMAREA with context/last map | 349 |

## Business Rules

### BR001: Account ID input must be exactly 11 numeric digits, non-zero; trim leading * or spaces to LOW-VALUES
**Logic:** If blank/* set BLANK flag and prompt; else test NUMERIC and <> ZEROES, else error msg and reprompt
**Conditions:** CC-ACCT-ID = LOW-VALUES/SPACES, CC-ACCT-ID NOT NUMERIC, CC-ACCT-ID = ZEROES
**Lines:** 628, 652, 666, 667

### BR002: Read chain fails if NOTFND in CXACAIX or ACCTDAT or CUSTDAT: set error flags, build specific msg with RESP/RESP2 and key, reprompt without data display
**Logic:** On DFHRESP(NOTFND) set INPUT-ERROR/FLG-NOT-OK, string msg with key/resp/reason into WS-RETURN-MSG if not already set
**Conditions:** WS-RESP-CD = DFHRESP(NOTFND)
**Lines:** 737, 742, 786, 790, 836, 840

### BR003: Only valid AIDs: ENTER or PF3; invalid PFK forces ENTER (reprompt)
**Logic:** Set PFK-VALID only for ENTER/PF3, else force ENTER
**Conditions:** CCARD-AID-ENTER, CCARD-AID-PFK03
**Lines:** 306, 307

### BR004: First entry (EIBCALEN=0) or menu call (CDEMO-FROM-PROGRAM=COMEN01C and not REENTER) initializes commarea and prompts
**Logic:** Initialize CARDDEMO-COMMAREA/WS-THIS-PROGCOMMAREA if new entry
**Conditions:** EIBCALEN = 0, CDEMO-FROM-PROGRAM = LIT-MENUPGM AND NOT CDEMO-PGM-REENTER
**Lines:** 282, 285

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [CVCRD01Y](../copybooks/CVCRD01Y.md) | WORKING_STORAGE | Card demo work area: CCARD-AID, CC-WORK-AREA, CCARD-ERROR-MSG, CCARD-NEXT-* fields | 207 |
| [COCOM01Y](../copybooks/COCOM01Y.md) | WORKING_STORAGE | CARDDEMO-COMMAREA: CDEMO-* fields for acct/cust/card IDs, from/to program/tranid, usrtag, pgm-enter/reenter, last-mapset/map | 211 |
| [DFHBMSCA](../copybooks/DFHBMSCA.md) | WORKING_STORAGE | BMS symbolic map constants: DFHBMFSE, DFHRED, DFHDFCOL etc. for attributes/color | 221 |
| [DFHAID](../copybooks/DFHAID.md) | WORKING_STORAGE | AID constants: CCARD-AID-ENTER, CCARD-AID-PFK03 etc. | 222 |
| [COTTL01Y](../copybooks/COTTL01Y.md) | WORKING_STORAGE | Screen titles: CCDA-TITLE01, CCDA-TITLE02 | 226 |
| [COACTVW](../copybooks/COACTVW.md) | WORKING_STORAGE | BMS map: CACTVWA I/O fields like ACCTSID*, ACSTTUS*, error/info msg | 229 |
| [CSDAT01Y](../copybooks/CSDAT01Y.md) | WORKING_STORAGE | Current date/time: WS-CURDATE-DATA, WS-CURTIME-* | 232 |
| [CSMSG01Y](../copybooks/CSMSG01Y.md) | WORKING_STORAGE | Common messages and literals | 235 |
| [CSMSG02Y](../copybooks/CSMSG02Y.md) | WORKING_STORAGE | Abend data: ABEND-MSG, ABEND-CULPRIT, ABEND-CODE, ABEND-DATA | 238 |
| [CSUSR01Y](../copybooks/CSUSR01Y.md) | WORKING_STORAGE | User data from signon | 241 |
| [CVACT01Y](../copybooks/CVACT01Y.md) | WORKING_STORAGE | ACCOUNT-RECORD layout: ACCT-* fields | 244 |
| [CVACT02Y](../copybooks/CVACT02Y.md) | WORKING_STORAGE | Additional account layout (unused in visible code) | 248 |
| [CVACT03Y](../copybooks/CVACT03Y.md) | WORKING_STORAGE | CARD-XREF-RECORD: XREF-* fields | 251 |
| [CVCUS01Y](../copybooks/CVCUS01Y.md) | WORKING_STORAGE | CUSTOMER-RECORD: CUST-* fields | 254 |

## Data Flow

### Reads From
- **CACTVWAI**: ACCTSIDI
  (Lines: 629)
- **CXACAIX (CARD-XREF-RECORD)**: XREF-CUST-ID, XREF-CARD-NUM
  (Lines: 739, 740)
- **ACCTDAT (ACCOUNT-RECORD)**: ACCT-ACTIVE-STATUS, ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT, ACCT-OPEN-DATE, ACCT-EXPIRAION-DATE, ACCT-REISSUE-DATE, ACCT-GROUP-ID
  (Lines: 471)
- **CUSTDAT (CUSTOMER-RECORD)**: CUST-ID, CUST-SSN, CUST-FICO-CREDIT-SCORE, CUST-DOB-YYYY-MM-DD, CUST-FIRST-NAME, CUST-MIDDLE-NAME, CUST-LAST-NAME, CUST-ADDR-LINE-1, CUST-ADDR-LINE-2, CUST-ADDR-LINE-3, CUST-ADDR-STATE-CD, CUST-ADDR-ZIP, CUST-ADDR-COUNTRY-CD, CUST-PHONE-NUM-1, CUST-PHONE-NUM-2, CUST-GOVT-ISSUED-ID, CUST-EFT-ACCOUNT-ID, CUST-PRI-CARD-HOLDER-IND
  (Lines: 494)

### Writes To
- **CACTVWAO**: ACCTSIDO, ACSTTUSO, ACURBALO, ACRDLIMO, ACSHLIMO, ACRCYCRO, ACRCYDBO, ADTOPENO, AEXPDTO, AREISDTO, AADDGRPO, ACSTNUMO, ACSTSSNO, ACSTFCOO, ACSTDOBO, ACSFNAMO, ACSMNAMO, ACSLNAMO, ACSADL1O, ACSADL2O, ACSCITYO, ACSSTTEO, ACSZIPCO, ACSCTRYO, ACSPHN1O, ACSPHN2O, ACSGOVTO, ACSEFTCO, ACSPFLGO, ERRMSGO, INFOMSGO
  (Lines: 468, 473, 495, 532, 533)
- **CARDDEMO-COMMAREA**: CDEMO-ACCT-ID, CDEMO-CUST-ID, CDEMO-CARD-NUM, CDEMO-TO-PROGRAM, CDEMO-TO-TRANID, CDEMO-FROM-TRANID, CDEMO-FROM-PROGRAM, CDEMO-LAST-MAPSET, CDEMO-LAST-MAP, CDEMO-USRTYP-USER, CDEMO-PGM-ENTER
  (Lines: 661, 676, 679, 330, 336, 341, 342, 346, 345)

### Transformations
- **ACCTSIDI** → **CC-ACCT-ID**: If * or SPACES, set to LOW-VALUES; else direct move
  (Lines: 628)
- **CUST-SSN** → **ACSTSSNO**: Format as XXX-XX-XXXX using STRING CUST-SSN(1:3)- (4:2)- (6:4)
  (Lines: 496)
- **WS-CURDATE-DATA** → **CURDATEO**: Extract MM-DD-YY from FUNCTION CURRENT-DATE
  (Lines: 443)

## Key Paragraphs

### 0000-MAIN
**Purpose:** Entry point: HANDLE ABEND, initialize areas/commarea, store PFKEY via COPY CSSTRPFY, validate/set AID, EVALUATE on AID/context to XCTL(PF3), send map(new), process inputs+read data(reenter), or plain text error
- Calls: YYYY-STORE-PFKEY, 1000-SEND-MAP, 2000-PROCESS-INPUTS, 9000-READ-ACCT, SEND-PLAIN-TEXT
- Lines: 262-393

### COMMON-RETURN
**Purpose:** Set error msg on map, build outgoing WS-COMMAREA from CARDDEMO-COMMAREA + WS-THIS-PROGCOMMAREA, CICS RETURN TRANSID CAVW
- Lines: 394-407

### 1000-SEND-MAP
**Purpose:** Chain to init vars/attrs/send screen map
- Calls: 1100-SCREEN-INIT, 1200-SETUP-SCREEN-VARS, 1300-SETUP-SCREEN-ATTRS, 1400-SEND-SCREEN
- Lines: 416-426

### 2000-PROCESS-INPUTS
**Purpose:** Chain receive map + edit inputs, set next prog/map/errormsg for return handling
- Calls: 2100-RECEIVE-MAP, 2200-EDIT-MAP-INPUTS
- Lines: 596-606

### 9000-READ-ACCT
**Purpose:** Set no info msg, chain read xref->acct->cust using acct ID, skip display if any NOTFND
- Calls: 9200-GETCARDXREF-BYACCT, 9300-GETACCTDATA-BYACCT, 9400-GETCUSTDATA-BYCUST
- Lines: 687-719

### ABEND-ROUTINE
**Purpose:** Default abend msg if none, set culprit, SEND ABEND-DATA NOHANDLE, cancel HANDLE, ABEND 9999
- Lines: 916-937

### SEND-PLAIN-TEXT
**Purpose:** Debug: SEND TEXT WS-RETURN-MSG ERASE/FREEKB, RETURN
- Lines: 877-887

## Error Handling

- **CICS READ RESP-CD = NOTFND or OTHER:** Set INPUT-ERROR true, FLG-ACCTFILTER-NOT-OK true, build WS-RETURN-MSG via STRING with op/file/key/resp/resp2 (if msg-off), reprompt via 1000-SEND-MAP
  (Lines: 737, 742, 759, 786, 790, 809, 836, 840, 858)
- **PFK-INVALID (non-ENTER/PF3 AID):** Force CCARD-AID-ENTER true, reprompt screen
  (Lines: 306, 312)
- **CICS ABEND (unhandled):** Trap via HANDLE LABEL ABEND-ROUTINE: send ABEND-DATA, ABEND ABCODE 9999
  (Lines: 264, 916, 924, 934)
- **Unexpected EVALUATE case:** Set ABEND-CODE 0001, WS-RETURN-MSG 'UNEXPECTED DATA SCENARIO', SEND-PLAIN-TEXT
  (Lines: 376, 379)

## CICS Operations

| Command | Resource | Purpose | Line |
|---------|----------|---------|------|
| SEND MAP | CCARD-NEXT-MAP (CACTVWA) | Send account view map from CACTVWAO with CURSOR/ERASE/FREEKB/RESP | 583 |
| RECEIVE MAP | LIT-THISMAP (CACTVWA) | Receive into CACTVWAI with RESP/RESP2 | 611 |
| READ | CXACAIX | Read card xref by acct RIDFLD with RESP/RESP2 into CARD-XREF-RECORD | 727 |
| READ | ACCTDAT | Read account master by acct RIDFLD with RESP/RESP2 into ACCOUNT-RECORD | 776 |
| READ | CUSTDAT | Read customer master by cust RIDFLD with RESP/RESP2 into CUSTOMER-RECORD | 826 |
| XCTL | CDEMO-TO-PROGRAM | PF3 exit to menu/caller with COMMAREA(CARDDEMO-COMMAREA) | 349 |
| RETURN |  | Return to CAVW TRANSID with COMMAREA(WS-COMMAREA) full length | 402 |
| HANDLE ABEND | ABEND-ROUTINE | Global exception handler | 264 |

## Open Questions

- **Exact field layouts in copybooks (e.g., lengths/types of CDEMO-ACCT-ID=11-digit, ACCT-CURR-BAL)**
  - Context: Assumed from usage (e.g., PIC 9(11) for acct ID from edits), but not explicitly defined in source
  - Suggestion: Analyze copybook source files like COCOM01Y, CVACT01Y
- **Implementation of YYYY-STORE-PFKEY (PERFORM at 299)**
  - Context: Invoked via PERFORM but defined in COPY 'CSSTRPFY' at 913 (not expanded)
  - Suggestion: Expand/analyze CSSTRPFY copybook
- **Purpose/usage of CVACT02Y (copied at 248)**
  - Context: Copied but no visible usage in source
  - Suggestion: Check if used in unshown copybook expansions

---
*Generated by War Rig WAR_RIG*