# COPAUS1C

**File**: `cbl/COPAUS1C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-26 14:21:56.414549

## Purpose

COPAUS1C is a CICS online program that provides a detailed view of pending authorization records from IMS database segments. It handles user interactions via BMS map COPAU1A, including loading details on ENTER/PF keys, toggling fraud status on PF5, navigating to next record on PF8, and returning to summary screen on PF3. Authorization details are populated from IMS PAUTSUM0 (summary) and PAUTDTL1 (details) segments, with fraud updates via linked program.

**Business Context**: CardDemo application - Authorization module for viewing and managing pending authorization details, including fraud marking.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO-COMMAREA | IOType.CICS_COMMAREA | Common area containing account ID, selected auth key, page info, fraud data from previous screens/programs. |
| COPAU1A | IOType.CICS_MAP | Input map for receiving user selections like selected auth key and AID keys. |
| PAUTSUM0 | IOType.IMS_SEGMENT | Pending Authorization Summary root segment, qualified by ACCNTID. |
| PAUTDTL1 | IOType.IMS_SEGMENT | Pending Authorization Details child segment, qualified by PAUT9CTS auth key. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output map populated with auth details, header, messages for screen display. |
| PAUTDTL1 | IOType.IMS_SEGMENT | Updated Pending Authorization Details segment after fraud toggle. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| COPAUS2C | CallType.CICS_LINK | To report or remove fraud status on the authorization record |
| COPAUS0C | CallType.CICS_XCTL | Return to authorization summary screen |

## Business Rules

- **BR001**: Toggle fraud status on PF5: if confirmed, remove; else report as fraud.
- **BR002**: Display response as A (green) if resp code '00', else D (red).
- **BR003**: Lookup decline reason description from table by resp reason code.
- **BR004**: Validate input for numeric ACCT-ID and non-blank selected key before read.

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point paragraph that orchestrates the entire program flow for the CICS transaction CPVD. It initializes flags and messages (lines 159-164), checks if first entry (EIBCALEN=0) to XCTL back to summary screen (165-170), or receives commarea and processes re-entry. On re-entry, it handles map receive and evaluates AID: ENTER/PF keys load details via PROCESS-ENTER-KEY (182), PF3 returns to summary (185), PF5 marks fraud (188), PF8 next record (191), other invalid keys show error (194). After processing, sends screen via SEND-AUTHVIEW-SCREEN (177,183,etc.) and CICS RETURN with commarea and same transid (202). Error flag and erase flag control screen behavior. No direct IMS access here, relies on subordinate paragraphs for data. Business logic centers on AID-driven navigation and validation. Errors from subordinates propagate via WS-MESSAGE and WS-ERR-FLG.

### PROCESS-ENTER-KEY
This paragraph handles loading the selected authorization record details on ENTER key or initial selection. It clears map input (210), validates CDEMO-ACCT-ID numeric and CDEMO-CPVD-PAU-SELECTED not blank/low-values (211-212), sets WS-ACCT-ID/WS-AUTH-KEY and performs READ-AUTH-RECORD if valid (213-216), else sets error flag (224). Unschedules PSB if needed (218-220). Always calls POPULATE-AUTH-DETAILS to refresh screen data from IMS segments (227). Inputs from commarea/map (CDEMO- fields), outputs updated map fields via populate. Business logic validates inputs before IMS read to prevent invalid queries. Errors set WS-ERR-FLG for message display. Calls READ-AUTH-RECORD for data fetch.

### MARK-AUTH-FRAUD
Triggered by PF5, this paragraph toggles fraud status on the selected auth record. Sets WS-ACCT-ID/KEY from commarea (231-232), reads current record via READ-AUTH-RECORD (234), toggles PA-FRAUD-CONFIRMED/REMOVED and sets WS-REPORT-FRAUD or WS-REMOVE-FRAUD (236-242). Prepares WS-FRAUD-DATA with record/cust data (244-246), CICS LINK to COPAUS2C (248-252). If normal and success, UPDATE-AUTH-DETAILS (255), else message/rollback (257,260). Refreshes selected key and repopulates details (264-266). Inputs: commarea selection, IMS current record; outputs: updated IMS segment, message. Business logic implements fraud toggle with linked program validation. Errors trigger ROLL-BACK and screen send.

### PROCESS-PF8-KEY
Handles PF8 for next authorization detail record. Sets keys from commarea (270-271), reads current via READ-AUTH-RECORD (273), then GNP next PAUTDTL1 (274). Unschedules PSB (276-279). If EOF, sets no-erase and last-auth message (281-285), else updates selected key and populates (286-288). Inputs: current selection from commarea, IMS positioning; outputs: next record details or EOF message to map. Business logic navigates sequentially via IMS GNP. Errors not explicitly handled here beyond caller.

### POPULATE-AUTH-DETAILS
Populates BMS output map COPAU1AO with IMS data if no error (294). Maps fields like CARDNUMO (295), reformats date/time (297-307), amount (308), response A/D with color (311-317), searches decline table for reason (319-328), other fields like codes/merchant (331-357), fraud date if flagged (344-350). Inputs: PENDING-AUTH-DETAILS from IMS read; outputs: all map O fields for display. Business logic formats data for screen, lookup reasons, conditional colors/text. No error handling, assumes valid data post-read. Called after every read/navigate.

### RETURN-TO-PREV-SCREEN
Prepares commarea for return to summary screen on PF3 or initial entry. Sets CDEMO-FROM-TRANID/PROGRAM/CONTEXT and CDEMO-PGM-ENTER (362-366), XCTL to COPAUS0C with commarea (367-370). Inputs: none specific; outputs: modified commarea. Business logic for navigation back. No data changes or errors.

### SEND-AUTHVIEW-SCREEN
Sends the BMS map COPAU1A with populated data. Calls POPULATE-HEADER-INFO first (375), sets message/length (377-379), conditional ERASE on flag (380-395). Inputs: populated COPAU1AO; outputs: screen display to user. Business logic controls erase for refresh vs update. Handles cursor positioning.

### RECEIVE-AUTHVIEW-SCREEN
Receives input from BMS map COPAU1A into COPAU1AI (400-406). Nohandle, assumes success. Inputs: user screen data; outputs: map I fields for processing. Called on re-entry.

### POPULATE-HEADER-INFO
Populates screen header with titles, tranid, program, current date/time formatted (411-429). Inputs: WS-CURDATE-DATA from FUNCTION; outputs: TITLEs, TRNNAMEO, etc. in map. Standard header setup.

### READ-AUTH-RECORD
Schedules PSB if needed (433), GU PAUTSUM0 by ACCT-ID into summary (439-445), evaluates IMS status: OK/not-EOF continue, else EOF/error message/send screen (447-462). If summary found, GNP child PAUTDTL1 by key (465-488), similar eval. Inputs: WS-ACCT-ID/KEY; outputs: PENDING-AUTH-SUMMARY/DETAILS populated. Business logic: qualified get unique/parent-child read. Errors: set flag/message/screen.

### READ-NEXT-AUTH-RECORD
GNP next PAUTDTL1 sibling (495-498), eval status: OK/not-EOF, EOF set flag, other error message/screen (500-517). Inputs: positioned PCB; outputs: next PENDING-AUTH-DETAILS. Sequential navigation.

### UPDATE-AUTH-DETAILS
REPL PAUTDTL1 with updated fraud data from WS-FRAUD-AUTH-RECORD (522-528), eval: OK syncpoint/set message (532-539), else rollback/error/screen (540-551). Inputs: WS-FRAUD-DATA post-link; outputs: updated IMS segment, message. Business logic: commit fraud changes.

### TAKE-SYNCPOINT
Exec CICS SYNCPOINT to commit changes (558). No error handling.

### ROLL-BACK
Exec CICS SYNCPOINT ROLLBACK to undo changes (566).

### SCHEDULE-PSB
DLI SCHD PSBPAUTB, handle TC by TERM/re-SCHD (575-589), set flag or error/message/screen on fail (590-602). Ensures PSB scheduled.

## Open Questions

- ? Exact field layouts and definitions in copybooks like CIPAUSMY, CIPAUDTY, COPAU01
  - Context: Copybooks not provided; inferred from usage but precise offsets/types unknown
- ? How COPAUS0C calls this program
  - Context: Inferred from context/WS-PGM-AUTH-SMRY but not explicit
- ? Full list of fields in CARDDEMO-COMMAREA
  - Context: Partial from COCOM01Y usage; others like CDEMO-CUST-ID assumed
