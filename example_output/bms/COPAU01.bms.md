# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-03-16 20:02:03.461289

## Purpose

This BMS map defines the screen layout for displaying pending authorization details in the CardDemo application. It includes fields for card number, authorization details (date, time, response, code), amount, POS entry mode, source, MCC code, card expiration date, authorization type, transaction ID, match status, fraud status, and merchant details (name, ID, city, state, zip). It also provides function key options for navigation and fraud management.

**Business Context**: This screen is used to view details of pending card authorization transactions, likely for review and potential fraud detection or resolution.

## Paragraphs/Procedures

### COPAU01
This DFHMSD macro defines the map set COPAU01. It specifies various attributes for the map set, including control options (ALARM, FREEKB), extended attributes (EXTATT=YES), language (LANG=COBOL), mode (MODE=INOUT), storage (STORAGE=AUTO), TIOAPFX (TIOAPFX=YES), and type (TYPE=&&SYSPARM). These attributes control the overall behavior and characteristics of the map set within the CICS environment. The CTRL parameters enable audible alarms and freeing of the keyboard after input. EXTATT enables extended attributes for color and highlighting. LANG specifies COBOL as the programming language. MODE specifies that the map can be used for both input and output. STORAGE specifies automatic storage management. TIOAPFX enables the TIOA prefix. TYPE specifies the map type based on the system parameter.

### COPAU1A
This DFHMDI macro defines the map COPAU1A within the COPAU01 mapset. It specifies the map's position and size on the screen. COLUMN=1 and LINE=1 indicate that the map starts at the top-left corner of the screen. SIZE=(24,80) defines the map as occupying the entire screen (24 lines by 80 columns). This macro essentially creates a container for all the fields defined within this map, determining the overall dimensions of the display. It acts as a parent to the individual DFHMDF field definitions, organizing them within the screen layout. The map provides a full-screen view for displaying authorization details.
