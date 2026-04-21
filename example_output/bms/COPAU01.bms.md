# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-04-21 13:47:27.099114

## Purpose

This BMS map defines the screen layout for displaying pending authorization details in the CardDemo application. It includes fields for card number, authorization date/time, response, amount, merchant details, and other relevant information. The screen also provides options to navigate back, mark/remove fraud, and view the next authorization.

**Business Context**: This screen is used to view and manage pending card authorization details, likely as part of a fraud monitoring or customer service application.

## Paragraphs/Procedures

### COPAU01
This DFHMSD macro defines the map set COPAU01, which encapsulates the screen definition. It specifies various attributes for the map set, including control options (ALARM, FREEKB), extended attributes (EXTATT=YES), language (LANG=COBOL), mode (MODE=INOUT), storage (STORAGE=AUTO), terminal I/O area prefix (TIOAPFX=YES), and type (TYPE=&&SYSPARM). The TYPE parameter allows specifying different map types during compilation (e.g., TYPE=MAP, TYPE=DSECT). The CTRL options enable the alarm and release the keyboard after input. EXTATT enables extended attributes like color. LANG specifies COBOL as the programming language. MODE=INOUT indicates the map can be used for both input and output. STORAGE=AUTO lets CICS manage storage. TIOAPFX=YES includes a prefix in the I/O area. This paragraph essentially sets up the overall environment and characteristics for the BMS map.

### COPAU1A
This DFHMDI macro defines the map COPAU1A within the COPAU01 mapset. It specifies the screen's dimensions and position. COLUMN=1 and LINE=1 place the map at the top-left corner of the screen. SIZE=(24,80) defines the screen as 24 lines by 80 columns, which is a standard terminal size. This paragraph essentially defines the overall screen layout and size for the application's display. It does not directly process any data or implement business logic but provides the foundation for displaying the various fields defined by the DFHMDF macros.
