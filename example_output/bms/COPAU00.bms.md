# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-03-16 20:02:31.501912

## Purpose

This BMS map defines the screen layout for the CardDemo application's Pending Authorization screen. It displays customer account information, credit and cash limits/balances, and a list of recent transactions awaiting authorization. The screen allows users to select a transaction for further action.

**Business Context**: This screen is used in a card authorization system to view and manage pending authorizations for customer accounts.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID entered by the user to search for pending authorizations. |
| SEL0001 - SEL0004 | IOType.CICS_MAP | Selection fields for each transaction. User selects a transaction by entering a value in this field. |

## Paragraphs/Procedures

### COPAU00
This DFHMSD macro defines the BMS map set COPAU00. It specifies various control options for the map set, including terminal control (ALARM, FREEKB), extended attributes (EXTATT=YES), the programming language (LANG=COBOL), input/output mode (MODE=INOUT), storage management (STORAGE=AUTO), the presence of a TIOAPFX (TIOAPFX=YES), and the map type based on the system parameter (TYPE=&&SYSPARM). The CTRL parameter enables the alarm and keyboard freeing features. The EXTATT parameter enables extended attributes such as color and highlighting. The LANG parameter specifies COBOL as the programming language. The MODE parameter specifies that the map can be used for both input and output. The STORAGE parameter specifies that storage for the map should be automatically managed. The TIOAPFX parameter indicates that the terminal input/output area prefix is present. The TYPE parameter determines the type of map generated based on the system parameter.

### COPAU0A
This DFHMDI macro defines the BMS map COPAU0A within the COPAU00 mapset. It specifies the screen's dimensions and position. The COLUMN and LINE parameters set the starting position of the map on the screen to row 1, column 1. The SIZE parameter defines the map's dimensions as 24 rows and 80 columns, effectively defining a standard-sized terminal screen. This macro acts as a container for the individual field definitions (DFHMDF macros) that make up the screen layout. It establishes the overall structure and size of the screen presented to the user.

## Open Questions

- ? What is the purpose of the TYPE=&&SYSPARM parameter in the DFHMSD macro?
  - Context: The meaning of &&SYSPARM is unclear without additional context.
