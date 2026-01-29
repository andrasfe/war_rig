# COPAU01 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** COPAU01
- **File Name:** COPAU01.bms
- **File Type:** BMS
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:54:17.485481

## Purpose

**Summary:** This BMS map defines the screen layout for displaying pending authorization details in the CardDemo application. It includes fields for transaction information, card details, authorization responses, amounts, merchant information, and navigation options.

**Business Context:** This screen is used to view and manage pending card authorizations, likely as part of a fraud detection or transaction monitoring system.
**Program Type:** ONLINE_CICS

## Paragraphs

### COPAU01 DFHMSD

This DFHMSD macro defines the BMS map set named COPAU01. It specifies various attributes for the map set, including control options (ALARM, FREEKB), extended attributes (EXTATT=YES), the programming language (LANG=COBOL), input/output mode (MODE=INOUT), storage allocation (STORAGE=AUTO), the presence of a TIOA prefix (TIOAPFX=YES), and the map type (TYPE=&&SYSPARM). The CTRL parameter enables the alarm and releases the keyboard after input. EXTATT enables extended attributes. LANG specifies COBOL as the programming language. MODE specifies that the map can be used for both input and output. STORAGE specifies automatic storage allocation. TIOAPFX indicates that a terminal input/output area (TIOA) prefix is used. TYPE specifies the map type, which is determined by the system parameter &&SYSPARM. This macro essentially sets up the overall environment and characteristics for the BMS map set.

### COPAU1A DFHMDI

This DFHMDI macro defines a map named COPAU1A within the COPAU01 mapset. It sets the column and line position to start at the top-left corner of the screen (COLUMN=1, LINE=1) and defines the screen size as 24 rows by 80 columns (SIZE=(24,80)). This macro defines the overall dimensions and placement of the screen display.

### Field Definitions (DFHMDF)

These DFHMDF macros define the individual fields that make up the screen layout. Each field definition specifies attributes such as display characteristics (ATTRB), color (COLOR), length (LENGTH), position (POS), and initial value (INITIAL). The fields are arranged to display transaction details, card information, authorization responses, merchant details, and navigation prompts. For example, TRNNAME displays the transaction name, CARDNUM displays the card number, and AUTHRSP displays the authorization response. The ERRMSG field is used to display error messages in red. The final fields display function key options for navigation. The ATTRB parameter controls field attributes like ASKIP (auto-skip), NORM (normal intensity), BRT (bright intensity), and FSET (modified data tag). The COLOR parameter sets the color of the field. The LENGTH parameter specifies the field length. The POS parameter defines the field position on the screen. The INITIAL parameter sets the initial value of the field.

## Data Flow

## Dead Code

- **COPAU01** (map): Artifact 'COPAU01' (map) is never referenced by any other artifact in the dependency graph
- **COPAU1A** (screen): Screen/Map 'COPAU1A' is never sent to or received from by any program
