# COPAUS0C

**File**: `cbl/COPAUS0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-02-25 15:36:04.043458

## Purpose

COPAUS0C is a CICS transaction program (CPVS) that displays a paginated summary list of up to 5 pending authorization transactions for a specified account ID using BMS map COPAU0A. It supports user interactions via ENTER to validate account/selection and XCTL to details, PF7/PF8 for backward/forward paging through IMS PAUTDTL1 segments, PF3 to return to menu. Authorization data is fetched from IMS database using DLI GNP calls, formatted, and displayed with status, date, time, amount.

**Business Context**: CardDemo application Authorization Module - provides summary view of authorization messages for accounts.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AI | IOType.CICS_MAP | BMS input map containing user-entered account ID (ACCTIDI), selection fields (SEL0001I-SEL0005I), and other screen inputs |
| DFHCOMMAREA | IOType.CICS_COMMAREA | CARDDEMO-COMMAREA holding persistent state like CDEMO-ACCT-ID, page number, previous auth keys (CDEMO-CPVS-PAUKEY-PREV-PG), selected auth, next page flag |
| PENDING-AUTH-DETAILS | IOType.IMS_SEGMENT | IMS database child segment PAUTDTL1 containing pending auth details like key, amount, date, time, resp code, transaction ID, type, match status |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | BMS output map populated with header (titles, program name, date, tran id), auth list details (TRNID*O, PDATE*O etc implied), error message (ERRMSGO), account id display |
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Updated with current page num, auth keys for paging, selected auth key/flag, acct id, context flags before RETURN |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS1C | CallType.CICS_XCTL | Transfer to authorization details display for user-selected summary line (when 'S' entered) |
| COMEN01C | CallType.CICS_XCTL | Return to main menu screen on PF3 |

## Business Rules

- **BR001**: Account ID entry must be numeric and non-blank
- **BR002**: Line selection must be 'S' or 's' to invoke details program
- **BR003**: PF7 backward only if current page > 1
- **BR004**: Approval status derived from response code '00' = Approved 'A' else Declined 'D'

## Paragraphs/Procedures

### MAIN-PARA
> [Source: MAIN-PARA.cbl.md](COPAUS0C.cbl.d/MAIN-PARA.cbl.md)

```
MAIN-PARA  (52 statements, depth=7)
PARAGRAPH
├── SET: SET ERR-FLG-OFF TO TRUE
├── SET: SET AUTHS-NOT-EOF TO TRUE
├── SET: SET NEXT-PAGE-NO TO TRUE
├── SET: SET SEND-ERASE-YES TO TRUE
├── MOVE: MOVE SPACES TO WS-MESSAGE ERRMSGO OF COPAU0AO
├── MOVE: MOVE -1       TO ACCTIDL OF COPAU0AI
├── IF: IF EIBCALEN = 0
│   ├── INITIALIZE: INITIALIZE CARDDEMO-COMMAREA
│   ├── MOVE: MOVE WS-PGM-AUTH-SMRY    TO CDEMO-TO-PROGRAM
│   ├── SET: SET CDEMO-PGM-REENTER    TO TRUE
│   ├── MOVE: MOVE LOW-VALUES          TO COPAU0AO
│   ├── MOVE: MOVE -1                  TO ACCTIDL OF COPAU0AI
│   ├── PERFORM: PERFORM SEND-PAULST-SCREEN
│   └── ELSE: ELSE
│       ├── MOVE: MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
│       └── IF: IF NOT CDEMO-PGM-REENTER
│           ├── SET: SET CDEMO-PGM-REENTER     TO TRUE
│           ├── MOVE: MOVE LOW-VALUES           TO COPAU0AO
│           ├── IF: IF CDEMO-ACCT-ID IS NUMERIC
│           │   ├── MOVE: MOVE CDEMO-ACCT-ID     TO WS-ACCT-ID ACCTIDO OF COPAU0AO
│           │   └── ELSE: ELSE
│           │       ├── MOVE: MOVE SPACE             TO ACCTIDO OF COPAU0AO
│           │       └── MOVE: MOVE LOW-VALUES        TO WS-ACCT-ID
│           ├── PERFORM: PERFORM GATHER-DETAILS
│           ├── SET: SET SEND-ERASE-YES TO TRUE
│           ├── PERFORM: PERFORM SEND-PAULST-SCREEN
│           └── ELSE: ELSE
│               ├── PERFORM: PERFORM RECEIVE-PAULST-SCREEN
│               └── EVALUATE: EVALUATE EIBAID
│                   ├── WHEN: WHEN DFHENTER
│                   │   ├── PERFORM: PERFORM PROCESS-ENTER-KEY
│                   │   ├── IF: IF WS-ACCT-ID = LOW-VALUES
│                   │   │   ├── MOVE: MOVE SPACE           TO ACCTIDO   OF COPAU0AO
│                   │   │   └── ELSE: ELSE
│                   │   │       └── MOVE: MOVE WS-ACCT-ID      TO ACCTIDO   OF COPAU0AO
│                   │   └── PERFORM: PERFORM SEND-PAULST-SCREEN
│                   ├── WHEN: WHEN DFHPF3
│                   │   ├── MOVE: MOVE WS-PGM-MENU        TO CDEMO-TO-PROGRAM
│                   │   ├── PERFORM: PERFORM RETURN-TO-PREV-SCREEN
│                   │   └── PERFORM: PERFORM SEND-PAULST-SCREEN
│                   ├── WHEN: WHEN DFHPF7
│                   │   ├── PERFORM: PERFORM PROCESS-PF7-KEY
│                   │   └── PERFORM: PERFORM SEND-PAULST-SCREEN
│                   ├── WHEN: WHEN DFHPF8
│                   │   ├── PERFORM: PERFORM PROCESS-PF8-KEY
│                   │   └── PERFORM: PERFORM SEND-PAULST-SCREEN
│                   └── WHEN: WHEN OTHER
│                       ├── MOVE: MOVE 'Y'              TO WS-ERR-FLG
│                       ├── MOVE: MOVE -1               TO ACCTIDL OF COPAU0AI
│                       ├── MOVE: MOVE CCDA-MSG-INVALID-KEY  TO WS-MESSAGE
│                       └── PERFORM: PERFORM SEND-PAULST-SCREEN
└── EXEC_CICS: EXEC CICS RETURN TRANSID (WS-CICS-TRANID) COMMAREA (CARDDEMO-COMMAREA...
```

This is the primary orchestration paragraph serving as the program entry point, handling initial load, reentry, and AID-based dispatching for the authorization summary screen. It consumes EIBCALEN, DFHCOMMAREA (on reentry), and map inputs via RECEIVE-PAULST-SCREEN, initializing flags (ERR-FLG, AUTHS-EOF, SEND-ERASE, NEXT-PAGE) and clearing messages/ACCTIDL. On first entry (EIBCALEN=0), initializes commarea with program name, low-values map, performs SEND-PAULST-SCREEN for blank screen. On reentry, loads commarea, sets reenter flag, optionally gathers details from prior ACCT-ID, sends screen; or receives map and dispatches: ENTER to process selection/acct, PF3 to menu via RETURN-TO-PREV-SCREEN, PF7/PF8 paging, other invalid AID error. Produces updated map output via SEND-PAULST-SCREEN and updated commarea for RETURN. Business logic includes reenter state management to distinguish initial vs subsequent calls. No explicit error handling beyond flag/message sets which trigger screen resend. Calls subordinate paragraphs for screen I/O, data gather, key processing, and XCTL return.

### PROCESS-ENTER-KEY
> [Source: PROCESS-ENTER-KEY.cbl.md](COPAUS0C.cbl.d/PROCESS-ENTER-KEY.cbl.md)

```
PROCESS-ENTER-KEY  (46 statements, depth=6)
PARAGRAPH
├── IF: IF ACCTIDI OF COPAU0AI = SPACES OR LOW-VALUES
│   ├── MOVE: MOVE LOW-VALUES                 TO WS-ACCT-ID
│   ├── MOVE: MOVE 'Y'                        TO WS-ERR-FLG
│   ├── MOVE: MOVE 'Please enter Acct Id...'       TO WS-MESSAGE
│   ├── MOVE: MOVE -1                         TO ACCTIDL OF COPAU0AI
│   └── ELSE: ELSE
│       └── IF: IF ACCTIDI OF COPAU0AI IS NOT NUMERIC
│           ├── MOVE: MOVE LOW-VALUES               TO WS-ACCT-ID
│           ├── MOVE: MOVE 'Y'                      TO WS-ERR-FLG
│           ├── MOVE: MOVE 'Acct Id must be Numeric ...' TO WS-MESSAGE
│           ├── MOVE: MOVE -1                       TO ACCTIDL OF COPAU0AI
│           └── ELSE: ELSE
│               ├── MOVE: MOVE ACCTIDI OF COPAU0AI      TO WS-ACCT-ID CDEMO-ACCT-ID
│               ├── EVALUATE: EVALUATE TRUE
│               │   ├── WHEN: WHEN SEL0001I OF COPAU0AI NOT = SPACES AND LOW-VALUES
│               │   │   ├── MOVE: MOVE SEL0001I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
│               │   │   └── MOVE: MOVE CDEMO-CPVS-AUTH-KEYS(1) TO CDEMO-CPVS-PAU-SELECTED
│               │   ├── WHEN: WHEN SEL0002I OF COPAU0AI NOT = SPACES AND LOW-VALUES
│               │   │   ├── MOVE: MOVE SEL0002I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
│               │   │   └── MOVE: MOVE CDEMO-CPVS-AUTH-KEYS(2) TO CDEMO-CPVS-PAU-SELECTED
│               │   ├── WHEN: WHEN SEL0003I OF COPAU0AI NOT = SPACES AND LOW-VALUES
│               │   │   ├── MOVE: MOVE SEL0003I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
│               │   │   └── MOVE: MOVE CDEMO-CPVS-AUTH-KEYS(3) TO CDEMO-CPVS-PAU-SELECTED
│               │   ├── WHEN: WHEN SEL0004I OF COPAU0AI NOT = SPACES AND LOW-VALUES
│               │   │   ├── MOVE: MOVE SEL0004I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
│               │   │   └── MOVE: MOVE CDEMO-CPVS-AUTH-KEYS(4) TO CDEMO-CPVS-PAU-SELECTED
│               │   ├── WHEN: WHEN SEL0005I OF COPAU0AI NOT = SPACES AND LOW-VALUES
│               │   │   ├── MOVE: MOVE SEL0005I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
│               │   │   └── MOVE: MOVE CDEMO-CPVS-AUTH-KEYS(5) TO CDEMO-CPVS-PAU-SELECTED
│               │   └── WHEN: WHEN OTHER
│               │       ├── MOVE: MOVE SPACES   TO CDEMO-CPVS-PAU-SEL-FLG
│               │       └── MOVE: MOVE SPACES   TO CDEMO-CPVS-PAU-SELECTED
│               └── IF: IF (CDEMO-CPVS-PAU-SEL-FLG NOT = SPACES AND LOW-VALUES) AND (CDEMO-CP...
│                   └── EVALUATE: EVALUATE CDEMO-CPVS-PAU-SEL-FLG
│                       ├── WHEN: WHEN 'S'
│                       ├── WHEN: WHEN 's'
│                       │   ├── MOVE: MOVE WS-PGM-AUTH-DTL  TO CDEMO-TO-PROGRAM
│                       │   ├── MOVE: MOVE WS-CICS-TRANID   TO CDEMO-FROM-TRANID
│                       │   ├── MOVE: MOVE WS-PGM-AUTH-SMRY TO CDEMO-FROM-PROGRAM
│                       │   ├── MOVE: MOVE 0                TO CDEMO-PGM-CONTEXT
│                       │   ├── SET: SET CDEMO-PGM-ENTER   TO TRUE
│                       │   └── EXEC_CICS: EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA) ...
│                       └── WHEN: WHEN OTHER
│                           ├── MOVE: MOVE 'Invalid selection. Valid value is S' TO WS-MESSAGE
│                           └── MOVE: MOVE -1                TO ACCTIDL OF COPAU0AI
└── PERFORM: PERFORM GATHER-DETAILS
```

This paragraph processes the ENTER keypress to validate and act on user input for account ID and line selection. It reads ACCTIDI and SEL000*I from COPAU0AI map, checks if ACCTIDI blank/low-values (error 'Please enter Acct Id'), or non-numeric (error 'Acct Id must be Numeric'). If valid, moves to WS-ACCT-ID/CDEMO-ACCT-ID, evaluates which SEL000*I non-blank/low to set CDEMO-CPVS-PAU-SEL-FLG and matching CDEMO-CPVS-AUTH-KEYS(n) to PAU-SELECTED. If valid selection and flag='S'/'s', sets XCTL to COPAUS1C details with context flags; else invalid selection error. Always performs GATHER-DETAILS post-validation to refresh data. Error handling sets WS-ERR-FLG, message, clears ACCTIDL display. No direct calls except GATHER-DETAILS; integrates with main flow for screen resend.

### GATHER-DETAILS
> [Source: GATHER-DETAILS.cbl.md](COPAUS0C.cbl.d/GATHER-DETAILS.cbl.md)

```
GATHER-DETAILS  (7 statements, depth=2)
PARAGRAPH
├── MOVE: MOVE -1       TO ACCTIDL OF COPAU0AI
├── MOVE: MOVE 0        TO CDEMO-CPVS-PAGE-NUM
└── IF: IF WS-ACCT-ID NOT = LOW-VALUES
    ├── PERFORM: PERFORM GATHER-ACCOUNT-DETAILS
    ├── PERFORM: PERFORM INITIALIZE-AUTH-DATA
    └── IF: IF FOUND-PAUT-SMRY-SEG
        └── PERFORM: PERFORM PROCESS-PAGE-FORWARD
```

Prepares data for screen display by resetting ACCTIDL display, page num to 0, and conditionally gathering account details and auth data. Consumes WS-ACCT-ID; if valid (not low-values), performs GATHER-ACCOUNT-DETAILS (missing code, likely sets account flags/validates existence), initializes auth list data, and if PAUT summary seg found, performs PROCESS-PAGE-FORWARD to load first page. Produces updated CDEMO-CPVS-PAGE-NUM=1+, auth keys in commarea for paging. Business logic gates paging on account-linked summary segment existence. Error handling inherited from callees via flags. Calls missing GATHER-ACCOUNT-DETAILS for acct validation and INITIALIZE-AUTH-DATA/PROCESS-PAGE-FORWARD for list population.

### PROCESS-PF7-KEY
> [Source: PROCESS-PF7-KEY.cbl.md](COPAUS0C.cbl.d/PROCESS-PF7-KEY.cbl.md)

```
PROCESS-PF7-KEY  (12 statements, depth=1)
PARAGRAPH
└── IF: IF CDEMO-CPVS-PAGE-NUM > 1
    ├── COMPUTE: COMPUTE CDEMO-CPVS-PAGE-NUM = CDEMO-CPVS-PAGE-NUM - 1
    ├── MOVE: MOVE CDEMO-CPVS-PAUKEY-PREV-PG(CDEMO-CPVS-PAGE-NUM) TO WS-AUTH-KEY-SAVE
    ├── PERFORM: PERFORM GET-AUTH-SUMMARY
    ├── SET: SET SEND-ERASE-NO            TO TRUE
    ├── SET: SET NEXT-PAGE-YES            TO TRUE
    ├── MOVE: MOVE -1                      TO ACCTIDL OF COPAU0AI
    ├── PERFORM: PERFORM INITIALIZE-AUTH-DATA
    ├── PERFORM: PERFORM PROCESS-PAGE-FORWARD
    └── ELSE: ELSE
        ├── MOVE: MOVE 'You are already at the top of the page...' TO WS-MESSAGE
        └── SET: SET SEND-ERASE-NO            TO TRUE
```

Handles PF7 (previous page) by decrementing CDEMO-CPVS-PAGE-NUM if >1, loading prior auth key from CDEMO-CPVS-PAUKEY-PREV-PG into WS-AUTH-KEY-SAVE, performing GET-AUTH-SUMMARY (missing, likely summary seg read), sets no-erase/next-page-yes, clears ACCTIDL, initializes list, processes forward page. If at page 1, sets top-of-list error message, no-erase. Consumes current page num/keys from commarea; produces repositioned page data, updated page num/keys. Business logic prevents backward from first page. Error handling via message/flag. Calls GET-AUTH-SUMMARY, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PF8-KEY
> [Source: PROCESS-PF8-KEY.cbl.md](COPAUS0C.cbl.d/PROCESS-PF8-KEY.cbl.md)

```
PROCESS-PF8-KEY  (13 statements, depth=1)
PARAGRAPH
├── IF: IF CDEMO-CPVS-PAUKEY-LAST = SPACES OR LOW-VALUES
│   ├── MOVE: MOVE LOW-VALUES             TO WS-AUTH-KEY-SAVE
│   └── ELSE: ELSE
│       ├── MOVE: MOVE CDEMO-CPVS-PAUKEY-LAST TO WS-AUTH-KEY-SAVE
│       ├── PERFORM: PERFORM GET-AUTH-SUMMARY
│       └── PERFORM: PERFORM REPOSITION-AUTHORIZATIONS
├── MOVE: MOVE -1                         TO ACCTIDL OF COPAU0AI
├── SET: SET SEND-ERASE-NO               TO TRUE
└── IF: IF NEXT-PAGE-YES
    ├── PERFORM: PERFORM INITIALIZE-AUTH-DATA
    ├── PERFORM: PERFORM PROCESS-PAGE-FORWARD
    └── ELSE: ELSE
        └── MOVE: MOVE 'You are already at the bottom of the page...' TO WS-MESSAGE
```

Handles PF8 (next/bottom page) by using CDEMO-CPVS-PAUKEY-LAST or low-values for WS-AUTH-KEY-SAVE, performs GET-AUTH-SUMMARY and REPOSITION-AUTHORIZATIONS to position at last known, clears ACCTIDL, no-erase. If next-page-yes after forward process, initializes and loads next page; else bottom-of-list message. Consumes last key/page flag from commarea; produces updated keys/page flag. Business logic checks for more data post-reposition. Error handling via flags/message. Calls GET-AUTH-SUMMARY, REPOSITION-AUTHORIZATIONS, INITIALIZE-AUTH-DATA, PROCESS-PAGE-FORWARD.

### PROCESS-PAGE-FORWARD
> [Source: PROCESS-PAGE-FORWARD.cbl.md](COPAUS0C.cbl.d/PROCESS-PAGE-FORWARD.cbl.md)

```
PROCESS-PAGE-FORWARD  (21 statements, depth=4)
PARAGRAPH
└── IF: IF ERR-FLG-OFF
    ├── MOVE: MOVE 1             TO  WS-IDX
    ├── MOVE: MOVE LOW-VALUES    TO CDEMO-CPVS-PAUKEY-LAST
    ├── PERFORM_INLINE: PERFORM UNTIL WS-IDX > 5 OR AUTHS-EOF OR ERR-FLG-ON
    │   ├── IF: IF EIBAID = DFHPF7 AND WS-IDX = 1
    │   │   ├── PERFORM: PERFORM REPOSITION-AUTHORIZATIONS
    │   │   └── ELSE: ELSE
    │   │       └── PERFORM: PERFORM GET-AUTHORIZATIONS
    │   └── IF: IF AUTHS-NOT-EOF AND ERR-FLG-OFF
    │       ├── PERFORM: PERFORM POPULATE-AUTH-LIST
    │       ├── COMPUTE: COMPUTE WS-IDX = WS-IDX + 1
    │       ├── MOVE: MOVE PA-AUTHORIZATION-KEY TO CDEMO-CPVS-PAUKEY-LAST
    │       └── IF: IF WS-IDX = 2
    │           ├── COMPUTE: COMPUTE CDEMO-CPVS-PAGE-NUM = CDEMO-CPVS-PAGE-NUM + 1
    │           └── MOVE: MOVE PA-AUTHORIZATION-KEY TO CDEMO-CPVS-PAUKEY-PREV-PG(CDEMO-CPVS-PAG...
    └── IF: IF AUTHS-NOT-EOF AND ERR-FLG-OFF
        ├── PERFORM: PERFORM GET-AUTHORIZATIONS
        └── IF: IF AUTHS-NOT-EOF AND ERR-FLG-OFF
            ├── SET: SET NEXT-PAGE-YES TO TRUE
            └── ELSE: ELSE
                └── SET: SET NEXT-PAGE-NO TO TRUE
```

Loads one page (5 auths) of data into map/commarea by looping WS-IDX 1-5 or EOF/error, conditionally repositioning on PF7 first item, calling GET-AUTHORIZATIONS to fetch next PAUTDTL1 seg, populates list if success, tracks last key and prev-pg keys/page num. After loop, peeks one more GET to set NEXT-PAGE-YES/NO. Consumes IMS PCB, current position/key; produces map fields (*I TRNID etc.), commarea keys/page/next-flag. Business logic stops at 5 or EOF/error, builds prev history. Error handling aborts loop on ERR-FLG-ON. Calls REPOSITION-AUTHORIZATIONS (conditional), GET-AUTHORIZATIONS, POPULATE-AUTH-LIST.

### GET-AUTHORIZATIONS
> [Source: GET-AUTHORIZATIONS.cbl.md](COPAUS0C.cbl.d/GET-AUTHORIZATIONS.cbl.md)

```
GET-AUTHORIZATIONS  (13 statements, depth=1)
PARAGRAPH
├── EXEC_DLI: EXEC DLI GNP USING PCB(PAUT-PCB-NUM) SEGMENT (PAUTDTL1) INTO (PENDING...
├── MOVE: MOVE DIBSTAT                          TO IMS-RETURN-CODE
└── EVALUATE: EVALUATE TRUE
    ├── WHEN: WHEN STATUS-OK
    │   └── SET: SET AUTHS-NOT-EOF              TO TRUE
    ├── WHEN: WHEN SEGMENT-NOT-FOUND
    ├── WHEN: WHEN END-OF-DB
    │   └── SET: SET AUTHS-EOF                  TO TRUE
    └── WHEN: WHEN OTHER
        ├── MOVE: MOVE 'Y'     TO WS-ERR-FLG
        ├── STRING: STRING ' System error while reading AUTH Details: Code:' IMS-RETURN-C...
        ├── MOVE: MOVE -1       TO ACCTIDL OF COPAU0AI
        └── PERFORM: PERFORM SEND-PAULST-SCREEN
```

Performs unqualified IMS DLI GNP call on PAUTDTL1 segment into PENDING-AUTH-DETAILS to fetch next sequential authorization detail. Evaluates IMS-RETURN-CODE (DIBSTAT): OK sets not-EOF, GE/GB sets EOF, others error flag/message with code and SEND screen. Consumes IMS PCB(PAUT-PCB-NUM); produces filled PENDING-AUTH-DETAILS or EOF/error. Business logic for sequential read post-GU/GNP position. Error handling immediate screen send on non-retry codes. No calls.

### REPOSITION-AUTHORIZATIONS
> [Source: REPOSITION-AUTHORIZATIONS.cbl.md](COPAUS0C.cbl.d/REPOSITION-AUTHORIZATIONS.cbl.md)

```
REPOSITION-AUTHORIZATIONS  (14 statements, depth=1)
PARAGRAPH
├── MOVE: MOVE WS-AUTH-KEY-SAVE          TO PA-AUTHORIZATION-KEY
├── EXEC_DLI: EXEC DLI GNP USING PCB(PAUT-PCB-NUM) SEGMENT (PAUTDTL1) INTO (PENDING...
├── MOVE: MOVE DIBSTAT                          TO IMS-RETURN-CODE
└── EVALUATE: EVALUATE TRUE
    ├── WHEN: WHEN STATUS-OK
    │   └── SET: SET AUTHS-NOT-EOF              TO TRUE
    ├── WHEN: WHEN SEGMENT-NOT-FOUND
    ├── WHEN: WHEN END-OF-DB
    │   └── SET: SET AUTHS-EOF                  TO TRUE
    └── WHEN: WHEN OTHER
        ├── MOVE: MOVE 'Y'     TO WS-ERR-FLG
        ├── STRING: STRING ' System error while repos. AUTH Details: Code:' IMS-RETURN-CO...
        ├── MOVE: MOVE -1       TO ACCTIDL OF COPAU0AI
        └── PERFORM: PERFORM SEND-PAULST-SCREEN
```

Repositions IMS cursor for paging by setting PA-AUTHORIZATION-KEY = WS-AUTH-KEY-SAVE, DLI GNP with WHERE on PAUT9CTS matching key into PENDING-AUTH-DETAILS. Evaluates IMS-RETURN-CODE: OK not-EOF, GE/GB EOF, others error message/SEND. Consumes WS-AUTH-KEY-SAVE, IMS PCB; produces positioned PENDING-AUTH-DETAILS or EOF/error. Business logic for exact key restart on page change. Error handling sends screen. No calls.

### POPULATE-AUTH-LIST
> [Source: POPULATE-AUTH-LIST.cbl.md](COPAUS0C.cbl.d/POPULATE-AUTH-LIST.cbl.md)

```
POPULATE-AUTH-LIST  (65 statements, depth=1)
PARAGRAPH
├── MOVE: MOVE PA-APPROVED-AMT           TO WS-AUTH-AMT
├── MOVE: MOVE PA-AUTH-ORIG-TIME(1:2)    TO WS-AUTH-TIME(1:2)
├── MOVE: MOVE PA-AUTH-ORIG-TIME(3:2)    TO WS-AUTH-TIME(4:2)
├── MOVE: MOVE PA-AUTH-ORIG-TIME(5:2)    TO WS-AUTH-TIME(7:2)
├── MOVE: MOVE PA-AUTH-ORIG-DATE(1:2)    TO WS-CURDATE-YY
├── MOVE: MOVE PA-AUTH-ORIG-DATE(3:2)    TO WS-CURDATE-MM
├── MOVE: MOVE PA-AUTH-ORIG-DATE(5:2)    TO WS-CURDATE-DD
├── MOVE: MOVE WS-CURDATE-MM-DD-YY       TO WS-AUTH-DATE
├── IF: IF PA-AUTH-RESP-CODE = '00'
│   ├── MOVE: MOVE 'A'               TO WS-AUTH-APRV-STAT
│   └── ELSE: ELSE
│       └── MOVE: MOVE 'D'               TO WS-AUTH-APRV-STAT
└── EVALUATE: EVALUATE WS-IDX
    ├── WHEN: WHEN 1
    │   ├── MOVE: MOVE PA-AUTHORIZATION-KEY TO CDEMO-CPVS-AUTH-KEYS(1)
    │   ├── MOVE: MOVE PA-TRANSACTION-ID TO TRNID01I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-DATE      TO PDATE01I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-TIME      TO PTIME01I OF COPAU0AI
    │   ├── MOVE: MOVE PA-AUTH-TYPE      TO PTYPE01I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-APRV-STAT TO PAPRV01I OF COPAU0AI
    │   ├── MOVE: MOVE PA-MATCH-STATUS   TO PSTAT01I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-AMT       TO PAMT001I OF COPAU0AI
    │   └── MOVE: MOVE DFHBMUNP          TO SEL0001A OF COPAU0AI
    ├── WHEN: WHEN 2
    │   ├── MOVE: MOVE PA-AUTHORIZATION-KEY TO CDEMO-CPVS-AUTH-KEYS(2)
    │   ├── MOVE: MOVE PA-TRANSACTION-ID TO TRNID02I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-DATE      TO PDATE02I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-TIME      TO PTIME02I OF COPAU0AI
    │   ├── MOVE: MOVE PA-AUTH-TYPE      TO PTYPE02I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-APRV-STAT TO PAPRV02I OF COPAU0AI
    │   ├── MOVE: MOVE PA-MATCH-STATUS   TO PSTAT02I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-AMT       TO PAMT002I OF COPAU0AI
    │   └── MOVE: MOVE DFHBMUNP          TO SEL0002A OF COPAU0AI
    ├── WHEN: WHEN 3
    │   ├── MOVE: MOVE PA-AUTHORIZATION-KEY TO CDEMO-CPVS-AUTH-KEYS(3)
    │   ├── MOVE: MOVE PA-TRANSACTION-ID TO TRNID03I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-DATE      TO PDATE03I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-TIME      TO PTIME03I OF COPAU0AI
    │   ├── MOVE: MOVE PA-AUTH-TYPE      TO PTYPE03I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-APRV-STAT TO PAPRV03I OF COPAU0AI
    │   ├── MOVE: MOVE PA-MATCH-STATUS   TO PSTAT03I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-AMT       TO PAMT003I OF COPAU0AI
    │   └── MOVE: MOVE DFHBMUNP          TO SEL0003A OF COPAU0AI
    ├── WHEN: WHEN 4
    │   ├── MOVE: MOVE PA-AUTHORIZATION-KEY TO CDEMO-CPVS-AUTH-KEYS(4)
    │   ├── MOVE: MOVE PA-TRANSACTION-ID TO TRNID04I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-DATE      TO PDATE04I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-TIME      TO PTIME04I OF COPAU0AI
    │   ├── MOVE: MOVE PA-AUTH-TYPE      TO PTYPE04I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-APRV-STAT TO PAPRV04I OF COPAU0AI
    │   ├── MOVE: MOVE PA-MATCH-STATUS   TO PSTAT04I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-AMT       TO PAMT004I OF COPAU0AI
    │   └── MOVE: MOVE DFHBMUNP          TO SEL0004A OF COPAU0AI
    ├── WHEN: WHEN 5
    │   ├── MOVE: MOVE PA-AUTHORIZATION-KEY TO CDEMO-CPVS-AUTH-KEYS(5)
    │   ├── MOVE: MOVE PA-TRANSACTION-ID TO TRNID05I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-DATE      TO PDATE05I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-TIME      TO PTIME05I OF COPAU0AI
    │   ├── MOVE: MOVE PA-AUTH-TYPE      TO PTYPE05I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-APRV-STAT TO PAPRV05I OF COPAU0AI
    │   ├── MOVE: MOVE PA-MATCH-STATUS   TO PSTAT05I OF COPAU0AI
    │   ├── MOVE: MOVE WS-AUTH-AMT       TO PAMT005I OF COPAU0AI
    │   └── MOVE: MOVE DFHBMUNP          TO SEL0005A OF COPAU0AI
    └── WHEN: WHEN OTHER
        └── CONTINUE: CONTINUE
```

Transforms current PENDING-AUTH-DETAILS into nth screen line (WS-IDX 1-5) and commarea CDEMO-CPVS-AUTH-KEYS(n): moves/formats amt/date/time/status, sets *I fields on COPAU0AI (TRNID01I-PAMT005I, PAPRV* etc.), unprotected attr SEL000nA. Tracks LAST key, prev-pg on page 2+. Consumes PENDING-AUTH-DETAILS, WS-IDX; produces map fields, commarea keys. Business logic derives status A/D from resp code '00', formats display. No validation/error, assumes valid seg. No calls.

### INITIALIZE-AUTH-DATA
> [Source: INITIALIZE-AUTH-DATA.cbl.md](COPAUS0C.cbl.d/INITIALIZE-AUTH-DATA.cbl.md)

```
INITIALIZE-AUTH-DATA  (49 statements, depth=2)
PARAGRAPH
└── PERFORM_INLINE: PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 5
    └── EVALUATE: EVALUATE WS-IDX
        ├── WHEN: WHEN 1
        │   ├── MOVE: MOVE DFHBMPRO TO SEL0001A OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO TRNID01I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PDATE01I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PTIME01I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PTYPE01I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PAPRV01I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PSTAT01I OF COPAU0AI
        │   └── MOVE: MOVE SPACES   TO PAMT001I OF COPAU0AI
        ├── WHEN: WHEN 2
        │   ├── MOVE: MOVE DFHBMPRO TO SEL0002A OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO TRNID02I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PDATE02I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PTIME02I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PTYPE02I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PAPRV02I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PSTAT02I OF COPAU0AI
        │   └── MOVE: MOVE SPACES   TO PAMT002I OF COPAU0AI
        ├── WHEN: WHEN 3
        │   ├── MOVE: MOVE DFHBMPRO TO SEL0003A OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO TRNID03I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PDATE03I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PTIME03I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PTYPE03I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PAPRV03I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PSTAT03I OF COPAU0AI
        │   └── MOVE: MOVE SPACES   TO PAMT003I OF COPAU0AI
        ├── WHEN: WHEN 4
        │   ├── MOVE: MOVE DFHBMPRO TO SEL0004A OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO TRNID04I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PDATE04I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PTIME04I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PTYPE04I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PAPRV04I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PSTAT04I OF COPAU0AI
        │   └── MOVE: MOVE SPACES   TO PAMT004I OF COPAU0AI
        ├── WHEN: WHEN 5
        │   ├── MOVE: MOVE DFHBMPRO TO SEL0005A OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO TRNID05I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PDATE05I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PTIME05I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PTYPE05I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PAPRV05I OF COPAU0AI
        │   ├── MOVE: MOVE SPACES   TO PSTAT05I OF COPAU0AI
        │   └── MOVE: MOVE SPACES   TO PAMT005I OF COPAU0AI
        └── WHEN: WHEN OTHER
            └── CONTINUE: CONTINUE
```

Clears auth list display for new page by varying WS-IDX 1-5, setting protected attr DFHBMPRO on SEL000nA, spaces on all *I fields (TRNID01I-PAMT005I etc.) of COPAU0AI. Consumes nothing specific; produces blanked map lines ready for population. Business logic full clear before load. No conditions/error. No calls.

### RETURN-TO-PREV-SCREEN
> [Source: RETURN-TO-PREV-SCREEN.cbl.md](COPAUS0C.cbl.d/RETURN-TO-PREV-SCREEN.cbl.md)

```
RETURN-TO-PREV-SCREEN  (6 statements, depth=1)
PARAGRAPH
├── IF: IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
│   └── MOVE: MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
├── MOVE: MOVE WS-CICS-TRANID  TO CDEMO-FROM-TRANID
├── MOVE: MOVE WS-PGM-AUTH-SMRY TO CDEMO-FROM-PROGRAM
├── MOVE: MOVE ZEROS           TO CDEMO-PGM-CONTEXT
└── EXEC_CICS: EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA) ...
```

Prepares XCTL return to previous program (PF3 handling): defaults CDEMO-TO-PROGRAM to 'COSGN00C' if blank, sets from-tran/program/context=0, XCTL with commarea. Consumes CDEMO-TO-PROGRAM (set to menu); produces control transfer. Business logic menu fallback. No error. No calls.

### SEND-PAULST-SCREEN
> [Source: SEND-PAULST-SCREEN.cbl.md](COPAUS0C.cbl.d/SEND-PAULST-SCREEN.cbl.md)

```
SEND-PAULST-SCREEN  (9 statements, depth=1)
PARAGRAPH
├── IF: IF IMS-PSB-SCHD
│   ├── SET: SET IMS-PSB-NOT-SCHD      TO TRUE
│   └── EXEC_CICS: EXEC CICS SYNCPOINT END-EXEC
├── PERFORM: PERFORM POPULATE-HEADER-INFO
├── MOVE: MOVE WS-MESSAGE TO ERRMSGO OF COPAU0AO
└── IF: IF SEND-ERASE-YES
    ├── EXEC_CICS: EXEC CICS SEND MAP('COPAU0A') MAPSET('COPAU00') FROM(COPAU0AO) ERASE ...
    └── ELSE: ELSE
        └── EXEC_CICS: EXEC CICS SEND MAP('COPAU0A') MAPSET('COPAU00') FROM(COPAU0AO) CURSOR...
```

Sends BMS map COPAU0A: if PSB scheduled SYNCPOINT first (unschedule), performs POPULATE-HEADER-INFO, moves message to ERRMSGO AO, conditional ERASE on SEND-ERASE-FLAG with CURSOR. Consumes flags, WS-MESSAGE, header data; produces screen display. Business logic commit before send, erase on init/refresh. Error via prior flags. Calls POPULATE-HEADER-INFO.

### RECEIVE-PAULST-SCREEN
> [Source: RECEIVE-PAULST-SCREEN.cbl.md](COPAUS0C.cbl.d/RECEIVE-PAULST-SCREEN.cbl.md)

```
RECEIVE-PAULST-SCREEN  (2 statements, depth=0)
PARAGRAPH
├── EXEC_CICS: EXEC CICS RECEIVE MAP('COPAU0A') MAPSET('COPAU00') INTO(COPAU0AI) RES...
└── UNKNOWN
```

Receives user input from BMS map COPAU0A into COPAU0AI, captures RESP/RESP2 codes (unused). Consumes screen data; produces filled COPAU0AI fields. No logic/error handling on RESP. No calls.

### POPULATE-HEADER-INFO
> [Source: POPULATE-HEADER-INFO.cbl.md](COPAUS0C.cbl.d/POPULATE-HEADER-INFO.cbl.md)

```
POPULATE-HEADER-INFO  (13 statements, depth=0)
PARAGRAPH
├── MOVE: MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA
├── MOVE: MOVE CCDA-TITLE01           TO TITLE01O OF COPAU0AO
├── MOVE: MOVE CCDA-TITLE02           TO TITLE02O OF COPAU0AO
├── MOVE: MOVE WS-CICS-TRANID         TO TRNNAMEO OF COPAU0AO
├── MOVE: MOVE WS-PGM-AUTH-SMRY       TO PGMNAMEO OF COPAU0AO
├── MOVE: MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
├── MOVE: MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
├── MOVE: MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY
├── MOVE: MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COPAU0AO
├── MOVE: MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
├── MOVE: MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
├── MOVE: MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS
└── MOVE: MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COPAU0AO
```

Populates static screen header: current date via FUNCTION CURRENT-DATE parsed to MM/DD/YY, titles from CSMSG, tran/program names. Consumes date func, WS vars; produces TITLE01O/02O, TRNNAMEO, PGMNAMEO, CURDATEO on COPAU0AO. No conditions/error. No calls.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| exec-001 | function | 254 | Function 'exec-001' is never called by any other artifact |
| exec-002 | function | 322 | Function 'exec-002' is never called by any other artifact |
| exec-003 | function | 461 | Function 'exec-003' is never called by any other artifact |
| exec-004 | function | 493 | Function 'exec-004' is never called by any other artifact |
| exec-005 | function | 674 | Function 'exec-005' is never called by any other artifact |
| exec-006 | function | 686 | Function 'exec-006' is never called by any other artifact |
| exec-007 | function | 695 | Function 'exec-007' is never called by any other artifact |
| exec-008 | function | 703 | Function 'exec-008' is never called by any other artifact |
| exec-009 | function | 715 | Function 'exec-009' is never called by any other artifact |
| exec-010 | function | 818 | Function 'exec-010' is never called by any other artifact |
| exec-011 | function | 869 | Function 'exec-011' is never called by any other artifact |
| exec-012 | function | 920 | Function 'exec-012' is never called by any other artifact |
| exec-013 | function | 973 | Function 'exec-013' is never called by any other artifact |
| exec-014 | function | 1002 | Function 'exec-014' is never called by any other artifact |
| exec-015 | function | 1008 | Function 'exec-015' is never called by any other artifact |
| exec-016 | function | 1011 | Function 'exec-016' is never called by any other artifact |
| COPAU0AI | record_layout | 17 | Record layout 'COPAU0AI' is never used by any program |
| COPAU0AO | record_layout | 391 | Record layout 'COPAU0AO' is never used by any program |
| DFHCOMMAREA | record_layout | 172 | Record layout 'DFHCOMMAREA' is never used by any program |
| PENDING-AUTH-DETAILS | record_layout | 164 | Record layout 'PENDING-AUTH-DETAILS' is never used by any program |
| PENDING-AUTH-SUMMARY | record_layout | 160 | Record layout 'PENDING-AUTH-SUMMARY' is never used by any program |
| WS-IMS-VARIABLES | record_layout | 74 | Record layout 'WS-IMS-VARIABLES' is never used by any program |
| WS-SWITCHES | record_layout | 93 | Record layout 'WS-SWITCHES' is never used by any program |
| WS-VARIABLES | record_layout | 32 | Record layout 'WS-VARIABLES' is never used by any program |

## Control Flow

```mermaid
flowchart TD
    %% Title: COPAUS0C.cbl
    GATHER_ACCOUNT_DETAILS["GATHER-ACCOUNT-DETAILS"]
    GET_AUTH_SUMMARY["GET-AUTH-SUMMARY"]
    GETACCTDATA_BYACCT["GETACCTDATA-BYACCT"]
    GETCARDXREF_BYACCT["GETCARDXREF-BYACCT"]
    GETCUSTDATA_BYCUST["GETCUSTDATA-BYCUST"]
    GATHER_DETAILS["GATHER-DETAILS"]
    INITIALIZE_AUTH_DATA["INITIALIZE-AUTH-DATA"]
    PROCESS_PAGE_FORWARD["PROCESS-PAGE-FORWARD"]
    SCHEDULE_PSB["SCHEDULE-PSB"]
    SEND_PAULST_SCREEN["SEND-PAULST-SCREEN"]
    GET_AUTHORIZATIONS["GET-AUTHORIZATIONS"]
    WS_ACCTFILENAME__ext[("WS-ACCTFILENAME")]
    WS_CARDXREFNAME_ACCT_PATH__ext[("WS-CARDXREFNAME-ACCT-PATH")]
    WS_CUSTFILENAME__ext[("WS-CUSTFILENAME")]
    MAIN_PARA["MAIN-PARA"]
    PROCESS_ENTER_KEY["PROCESS-ENTER-KEY"]
    PROCESS_PF7_KEY["PROCESS-PF7-KEY"]
    PROCESS_PF8_KEY["PROCESS-PF8-KEY"]
    RECEIVE_PAULST_SCREEN["RECEIVE-PAULST-SCREEN"]
    RETURN_TO_PREV_SCREEN["RETURN-TO-PREV-SCREEN"]
    POPULATE_AUTH_LIST["POPULATE-AUTH-LIST"]
    POPULATE_HEADER_INFO["POPULATE-HEADER-INFO"]
    CDEMO_TO_PROGRAM__ext(["CDEMO-TO-PROGRAM"])
    REPOSITION_AUTHORIZATIONS["REPOSITION-AUTHORIZATIONS"]
    GATHER_ACCOUNT_DETAILS --> GET_AUTH_SUMMARY
    GATHER_ACCOUNT_DETAILS --> GETACCTDATA_BYACCT
    GATHER_ACCOUNT_DETAILS --> GETCARDXREF_BYACCT
    GATHER_ACCOUNT_DETAILS --> GETCUSTDATA_BYCUST
    GATHER_DETAILS --> GATHER_ACCOUNT_DETAILS
    GATHER_DETAILS --> INITIALIZE_AUTH_DATA
    GATHER_DETAILS --> PROCESS_PAGE_FORWARD
    GET_AUTH_SUMMARY --> SCHEDULE_PSB
    GET_AUTH_SUMMARY --> SEND_PAULST_SCREEN
    GET_AUTHORIZATIONS --> SEND_PAULST_SCREEN
    GETACCTDATA_BYACCT --> SEND_PAULST_SCREEN
    GETACCTDATA_BYACCT -.->|reads| WS_ACCTFILENAME__ext
    GETCARDXREF_BYACCT --> SEND_PAULST_SCREEN
    GETCARDXREF_BYACCT -.->|reads| WS_CARDXREFNAME_ACCT_PATH__ext
    GETCUSTDATA_BYCUST --> SEND_PAULST_SCREEN
    GETCUSTDATA_BYCUST -.->|reads| WS_CUSTFILENAME__ext
    MAIN_PARA --> GATHER_DETAILS
    MAIN_PARA --> PROCESS_ENTER_KEY
    MAIN_PARA --> PROCESS_PF7_KEY
    MAIN_PARA --> PROCESS_PF8_KEY
    MAIN_PARA --> RECEIVE_PAULST_SCREEN
    MAIN_PARA --> RETURN_TO_PREV_SCREEN
    MAIN_PARA --> SEND_PAULST_SCREEN
    PROCESS_ENTER_KEY --> GATHER_DETAILS
    PROCESS_ENTER_KEY -.->|calls| CDEMO_TO_PROGRAM__ext
    PROCESS_PAGE_FORWARD --> GET_AUTHORIZATIONS
    PROCESS_PAGE_FORWARD --> POPULATE_AUTH_LIST
    PROCESS_PAGE_FORWARD --> REPOSITION_AUTHORIZATIONS
    PROCESS_PF7_KEY --> GET_AUTH_SUMMARY
    PROCESS_PF7_KEY --> INITIALIZE_AUTH_DATA
    PROCESS_PF7_KEY --> PROCESS_PAGE_FORWARD
    PROCESS_PF8_KEY --> GET_AUTH_SUMMARY
    PROCESS_PF8_KEY --> INITIALIZE_AUTH_DATA
    PROCESS_PF8_KEY --> PROCESS_PAGE_FORWARD
    PROCESS_PF8_KEY --> REPOSITION_AUTHORIZATIONS
    REPOSITION_AUTHORIZATIONS --> SEND_PAULST_SCREEN
    RETURN_TO_PREV_SCREEN -.->|calls| CDEMO_TO_PROGRAM__ext
    SCHEDULE_PSB --> SEND_PAULST_SCREEN
    SEND_PAULST_SCREEN --> POPULATE_HEADER_INFO
```

## Open Questions

- ? Definitions/code for paragraphs: GATHER-ACCOUNT-DETAILS, GET-AUTH-SUMMARY, GETACCTDATA-BYACCT, GETCARDXREF-BYACCT, GETCUSTDATA-BYCUST, SCHEDULE-PSB
  - Context: Source input truncated; these are PERFORMed but not provided, likely handle VSAM/IMS reads for ACCTDAT/CUSTDAT/CARDDAT/CXACAIX/CCXREF files, PSB schedule, summary seg fetch, set FOUND-* flags
- ? Role/purpose of VSAM-like filenames (ACCTDAT, CUSTDAT etc.) and related flags (WS-ACCT-MASTER-READ-FLG etc.)
  - Context: No FILE SECTION or VSAM I/O visible; likely in missing GATHER-ACCOUNT-DETAILS
- ? How PAUT summary segment read (FOUND-PAUT-SMRY-SEG set)?
  - Context: GNP on details only visible; summary likely GNP on root PAUTSMY1 or similar
- ? CICS RESP/RESP2 from RECEIVE handling
  - Context: Captured but not evaluated

## Sequence Diagram

```mermaid
sequenceDiagram
    participant MAIN_PARA as MAIN-PARA
    participant SEND_PAULST_SCREEN as SEND-PAULST-SCREEN
    participant GATHER_DETAILS as GATHER-DETAILS
    participant RECEIVE_PAULST_SCREEN as RECEIVE-PAULST-SCREEN
    participant PROCESS_ENTER_KEY as PROCESS-ENTER-KEY
    participant RETURN_TO_PREV_SCREEN as RETURN-TO-PREV-SCREEN
    participant PROCESS_PF7_KEY as PROCESS-PF7-KEY
    participant PROCESS_PF8_KEY as PROCESS-PF8-KEY
    participant CDEMO_TO_PROGRAM as CDEMO-TO-PROGRAM
    participant GATHER_ACCOUNT_DETAILS as GATHER-ACCOUNT-DETAILS
    participant INITIALIZE_AUTH_DATA as INITIALIZE-AUTH-DATA
    participant PROCESS_PAGE_FORWARD as PROCESS-PAGE-FORWARD
    participant GET_AUTH_SUMMARY as GET-AUTH-SUMMARY
    participant REPOSITION_AUTHORIZATIONS as REPOSITION-AUTHORIZATIONS
    participant GET_AUTHORIZATIONS as GET-AUTHORIZATIONS
    participant POPULATE_AUTH_LIST as POPULATE-AUTH-LIST
    participant POPULATE_HEADER_INFO as POPULATE-HEADER-INFO
    MAIN_PARA->>SEND_PAULST_SCREEN: performs
    MAIN_PARA->>GATHER_DETAILS: performs
    MAIN_PARA->>SEND_PAULST_SCREEN: performs
    MAIN_PARA->>RECEIVE_PAULST_SCREEN: performs
    MAIN_PARA->>PROCESS_ENTER_KEY: performs
    MAIN_PARA->>SEND_PAULST_SCREEN: performs
    MAIN_PARA->>RETURN_TO_PREV_SCREEN: performs
    MAIN_PARA->>SEND_PAULST_SCREEN: performs
    MAIN_PARA->>PROCESS_PF7_KEY: performs
    MAIN_PARA->>SEND_PAULST_SCREEN: performs
    MAIN_PARA->>PROCESS_PF8_KEY: performs
    MAIN_PARA->>SEND_PAULST_SCREEN: performs
    MAIN_PARA->>SEND_PAULST_SCREEN: performs
    PROCESS_ENTER_KEY->>GATHER_DETAILS: performs
    PROCESS_ENTER_KEY->>CDEMO_TO_PROGRAM: performs
    GATHER_DETAILS->>GATHER_ACCOUNT_DETAILS: performs
    GATHER_DETAILS->>INITIALIZE_AUTH_DATA: performs
    GATHER_DETAILS->>PROCESS_PAGE_FORWARD: performs
    PROCESS_PF7_KEY->>GET_AUTH_SUMMARY: performs
    PROCESS_PF7_KEY->>INITIALIZE_AUTH_DATA: performs
    PROCESS_PF7_KEY->>PROCESS_PAGE_FORWARD: performs
    PROCESS_PF8_KEY->>GET_AUTH_SUMMARY: performs
    PROCESS_PF8_KEY->>REPOSITION_AUTHORIZATIONS: performs
    PROCESS_PF8_KEY->>INITIALIZE_AUTH_DATA: performs
    PROCESS_PF8_KEY->>PROCESS_PAGE_FORWARD: performs
    PROCESS_PAGE_FORWARD->>REPOSITION_AUTHORIZATIONS: performs
    PROCESS_PAGE_FORWARD->>GET_AUTHORIZATIONS: performs
    PROCESS_PAGE_FORWARD->>POPULATE_AUTH_LIST: performs
    PROCESS_PAGE_FORWARD->>GET_AUTHORIZATIONS: performs
    GET_AUTHORIZATIONS->>SEND_PAULST_SCREEN: performs
    REPOSITION_AUTHORIZATIONS->>SEND_PAULST_SCREEN: performs
    RETURN_TO_PREV_SCREEN->>CDEMO_TO_PROGRAM: performs
    SEND_PAULST_SCREEN->>POPULATE_HEADER_INFO: performs
```
