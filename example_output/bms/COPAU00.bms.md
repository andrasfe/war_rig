# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-04-21 13:47:50.316565

## Purpose

This BMS map defines the screen layout for the CardDemo Pending Authorization screen. It displays customer account information, credit and cash limits/balances, and a list of recent transactions with their details (ID, date, time, type, approval status, and amount). The screen allows users to search for an account ID and select a transaction for further action.

**Business Context**: This screen is used to view and manage pending card authorizations within the CardDemo application.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID entered by the user to search for customer information. |
| SEL0001 - SEL0004 | IOType.CICS_MAP | Selection fields for each transaction to allow the user to select a transaction. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | The entire screen layout, including labels, data fields, and transaction details, sent to the user's terminal. |
| TRNNAME | IOType.CICS_MAP | Transaction Name |
| TITLE01 | IOType.CICS_MAP | Title of the screen |
| CURDATE | IOType.CICS_MAP | Current Date |
| PGMNAME | IOType.CICS_MAP | Program Name |
| TITLE02 | IOType.CICS_MAP | Secondary Title |
| CURTIME | IOType.CICS_MAP | Current Time |
| CNAME | IOType.CICS_MAP | Customer Name |
| CUSTID | IOType.CICS_MAP | Customer ID |
| ADDR001 | IOType.CICS_MAP | Address Line 1 |
| ACCSTAT | IOType.CICS_MAP | Account Status |
| ADDR002 | IOType.CICS_MAP | Address Line 2 |
| PHONE1 | IOType.CICS_MAP | Phone Number |
| APPRCNT | IOType.CICS_MAP | Approval Count |
| DECLCNT | IOType.CICS_MAP | Decline Count |
| CREDLIM | IOType.CICS_MAP | Credit Limit |
| CASHLIM | IOType.CICS_MAP | Cash Limit |
| APPRAMT | IOType.CICS_MAP | Approved Amount |
| CREDBAL | IOType.CICS_MAP | Credit Balance |
| CASHBAL | IOType.CICS_MAP | Cash Balance |
| DECLAMT | IOType.CICS_MAP | Declined Amount |
| TRNID01 - TRNID05 | IOType.CICS_MAP | Transaction ID for each of the 5 transactions displayed. |
| PDATE01 - PDATE05 | IOType.CICS_MAP | Transaction Date for each of the 5 transactions displayed. |
| PTIME01 - PTIME05 | IOType.CICS_MAP | Transaction Time for each of the 5 transactions displayed. |
| PTYPE01 - PTYPE05 | IOType.CICS_MAP | Transaction Type for each of the 5 transactions displayed. |
| PAPRV01 - PAPRV05 | IOType.CICS_MAP | Transaction Approval Status for each of the 5 transactions displayed. |
| PSTAT01 - PSTAT05 | IOType.CICS_MAP | Transaction Status for each of the 5 transactions displayed. |
| PAMT001 - PAMT004 | IOType.CICS_MAP | Transaction Amount for each of the 5 transactions displayed. |

## Paragraphs/Procedures

### COPAU00
This paragraph defines the BMS mapset COPAU00, which represents the entire screen definition. It specifies the overall characteristics of the screen, such as the control options (ALARM, FREEKB), extended attributes (EXTATT=YES), programming language (LANG=COBOL), input/output mode (MODE=INOUT), storage type (STORAGE=AUTO), terminal I/O area prefix (TIOAPFX=YES), and the map type based on the system parameter (TYPE=&&SYSPARM). The mapset acts as a container for individual map definitions (DFHMDI) and field definitions (DFHMDF), which collectively define the layout and attributes of the screen elements. It essentially sets the stage for the screen's appearance and behavior within the CICS environment, enabling the application to interact with the user through a structured display.

### COPAU0A
This paragraph defines the BMS map COPAU0A, which is a specific instance of the screen layout within the COPAU00 mapset. It defines the size and position of the screen on the terminal, specifying that the map occupies the entire 24x80 screen (SIZE=(24,80)) and starts at the top-left corner (LINE=1, COLUMN=1). This map definition acts as a container for the individual field definitions (DFHMDF) that make up the screen's content. It provides the overall structure and dimensions within which the various screen elements, such as labels, input fields, and output fields, are arranged and displayed to the user. The map COPAU0A is the primary interface through which the application presents information and receives input from the user.
