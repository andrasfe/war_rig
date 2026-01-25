# COPAUA0C

**File**: `cbl/COPAUA0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-25 18:31:40.746201

## Purpose

CICS COBOL program for card authorization decisions in CardDemo application. Processes pending authorization requests from MQ queue, performs lookups in files and IMS database, determines approve/decline status. Sends authorization response to MQ reply queue.

**Business Context**: Card authorization module handling transaction validation, balance checks, and fraud detection in a demo payment system

## Inputs

| Name | Type | Description |
|------|------|-------------|
| WS-REQUEST-QNAME | IOType.OTHER | MQ request queue containing pending authorization requests |
| PAUT-PCB | IOType.IMS_SEGMENT | IMS database segments for pending authorization summary and details via PSB PAUTB |
| CCXREF | IOType.FILE_VSAM | Card cross-reference file |
| ACCTDAT | IOType.FILE_VSAM | Account master file |
| CUSTDAT | IOType.FILE_VSAM | Customer master file |
| CARDDAT | IOType.FILE_VSAM | Card data file with alternate index CARDAIX |
| DFHCOMMAREA | IOType.CICS_COMMAREA | CICS linkage area for transaction data |
| MQTM | CICS_COMMAREA | Trigger message containing request queue name (MQTM-QNAME) and trigger data (MQTM-TRIGGERDATA) |
| WS-REQUEST-QNAME (MQ Request Queue) | OTHER | MQ messages with comma-delimited authorization request data: date, time, card num, auth type, expiry, msg type/source, processing code, transaction amt, merchant details, etc. |
| WS-CCXREF-FILE | FILE_VSAM | Card cross-reference records keyed by card number (XREF-CARD-NUM) |
| WS-ACCTFILENAME | FILE_VSAM | Account master records keyed by account ID (WS-CARD-RID-ACCT-ID-X) |
| WS-CUSTFILENAME | FILE_VSAM | Customer master records keyed by customer ID (WS-CARD-RID-CUST-ID-X) |
| PAUT-PCB-NUM (PAUTSUM0 segment) | IMS_SEGMENT | Pending auth summary segment qualified by account ID (ACCNTID) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| WS-REPLY-QNAME | IOType.OTHER | MQ reply queue for authorization responses |
| PAUT-PCB | IOType.IMS_SEGMENT | IMS updates to pending authorization summary and details segments |
| ERROR-LOG | IOType.OTHER | Application error log records |
| WS-REPLY-QNAME (MQ Reply Queue) | OTHER | Comma-delimited response: card num, transaction ID, auth ID code, resp code (00/05), reason, approved amt |
| CSSL | CICS_QUEUE | Error log records with location, codes, message, event key, etc. |
| PAUT-PCB-NUM (PAUTSUM0/PAUTDTL1 segments) | IMS_SEGMENT | Updated/inserted pending auth summary and details |

## Business Rules

- **BR001**: Decline authorization if insufficient funds
- **BR002**: Decline if card not active
- **BR003**: Decline if account closed
- **BR004**: Decline if card or merchant fraud detected
- **BR005**: Decline authorization if transaction amount exceeds available credit (credit limit minus balance) from pending auth summary if found, else from account master.
- **BR006**: Set authorization response code '00' for approve or '05' for decline; approved amt is transaction amt or 0.
- **BR007**: Assign decline reason codes based on failure conditions: 3100 for not found records, 4100 insufficient funds, 4200/4300/5100/5200 for other statuses/fraud.

## Paragraphs/Procedures

### MAIN-PARA
Main control: initialize, process loop, terminate, CICS RETURN

### 1000-INITIALIZE
Retrieve commarea for queue info, set wait interval, open request queue, read first MQ msg

### 2000-MAIN-PROCESS
Loop: extract msg, process auth, syncpoint, read next until limit or no more msgs

### 5000-PROCESS-AUTH
Read xref/acct/cust/summary/profile (stub), make decision, send response, write to DB if xref found

### 6000-MAKE-DECISION
Apply limit check rule, set approve/decline resp code/reason, build response string

### 9000-TERMINATE
TERM IMS PSB if scheduled, close request queue

### 9500-LOG-ERROR
Format timestamp, populate error record, WRITEQ to CSSL, abend if critical

### 9990-END-ROUTINE
Handles program termination by performing 9000-TERMINATE and executing CICS RETURN

### 9990-EXIT
Provides EXIT statement at end of 9990-END-ROUTINE

## Open Questions

- ? Exact I/O operations (OPEN, READ, WRITE, MQ GET/PUT, IMS GU/GN/IS calls)
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Paragraphs and control flow (PERFORMs, GO TOs)
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? File definitions (SELECT, FD, file status keys)
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Called programs or subroutines
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Precise business logic implementation for rules
  - Context: From chunk SOURCE-chunk-1-header (lines 1-217)
- ? Full data division and copybook layouts
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? Details of 5600-READ-PROFILE-DATA
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? 9990-END-ROUTINE implementation
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? MQ connection handle W01-HCONN-REQUEST initialization
  - Context: From chunk SOURCE-chunk-2 (lines 220-1015)
- ? What is the overall purpose of the COPAUA0C program?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
- ? What does the 9000-TERMINATE paragraph do?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
- ? What are the inputs, outputs, copybooks, and business rules?
  - Context: From chunk SOURCE-chunk-3 (lines 1016-1027)
