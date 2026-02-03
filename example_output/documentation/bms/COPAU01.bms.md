# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-02-03 21:07:34.553405

## Purpose

This BMS map defines the screen layout for the "Pending Authorization Details" screen in the CardDemo application. It displays information related to a card authorization, including card number, authorization details, merchant information, and fraud status. The screen also includes options for navigating back, marking/removing fraud, and viewing the next authorization.

**Business Context**: This screen is used to view and manage pending card authorizations, likely as part of a fraud detection or authorization management system.

## Paragraphs/Procedures

### COPAU1A DFHMDI
This DFHMDI macro defines the map's characteristics, including its size and position on the screen. It sets the column and line position to 1, indicating the top-left corner of the screen. The size is defined as 24 lines by 80 columns, representing the standard screen dimensions. This definition is crucial for establishing the overall layout and boundaries within which the other fields will be positioned. The map definition also specifies that the terminal I/O area prefix is enabled via TIOAPFX=YES (line 24).

### TRNNAME DFHMDF
This DFHMDF macro defines a display field for the transaction name. It is positioned at line 1, column 7, with a length of 4 characters. The field's attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 35). This field displays the transaction identifier associated with the authorization being viewed.

### TITLE01 DFHMDF
This DFHMDF macro defines a display field for the screen title. It is positioned at line 1, column 21, with a length of 40 characters. The field's attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to YELLOW (line 39). This field displays the main title of the screen, providing context to the user.

### CURDATE DFHMDF
This DFHMDF macro defines a display field for the current date. It is positioned at line 1, column 71, with a length of 8 characters. The field's attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 48). The field is initialized with 'mm/dd/yy' (line 51) as a placeholder. This field displays the current date for reference.

### PGMNAME DFHMDF
This DFHMDF macro defines a display field for the program name. It is positioned at line 2, column 7, with a length of 8 characters. The field's attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 58). This field displays the name of the program associated with the screen.

### TITLE02 DFHMDF
This DFHMDF macro defines a display field for a secondary screen title. It is positioned at line 2, column 21, with a length of 40 characters. The field's attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to YELLOW (line 62). This field displays a secondary title or additional information about the screen's purpose.

### CURTIME DFHMDF
This DFHMDF macro defines a display field for the current time. It is positioned at line 2, column 71, with a length of 8 characters. The field's attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 71). The field is initialized with 'hh:mm:ss' (line 74) as a placeholder. This field displays the current time for reference.

### View Authorization Details (Literal)
This DFHMDF macro defines a static text field displaying the literal 'View Authorization Details'. It is positioned at line 4, column 27, with a length of 26 characters. The attributes are set to ASKIP and BRT, meaning the cursor will skip over this field, and it will be displayed with bright intensity. The color is set to NEUTRAL (line 76). This field serves as a prominent heading for the screen's main content.

### CARDNUM DFHMDF
This DFHMDF macro defines a display field for the card number. It is positioned at line 7, column 11, with a length of 16 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to PINK (line 86). This field displays the card number associated with the authorization.

### AUTHDT DFHMDF
This DFHMDF macro defines a display field for the authorization date. It is positioned at line 7, column 43, with a length of 10 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to PINK (line 95). The field is initialized with a space (line 98). This field displays the date when the authorization was processed.

### AUTHTM DFHMDF
This DFHMDF macro defines a display field for the authorization time. It is positioned at line 7, column 68, with a length of 10 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to PINK (line 105). The field is initialized with a space (line 108). This field displays the time when the authorization was processed.

### AUTHRSP DFHMDF
This DFHMDF macro defines a display field for the authorization response code. It is positioned at line 9, column 14, with a length of 1 character. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to PINK (line 115). The field is initialized with a space (line 118). This field displays the response code received from the authorization system.

### AUTHRSN DFHMDF
This DFHMDF macro defines a display field for the authorization response reason. It is positioned at line 9, column 32, with a length of 20 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 125). The field is initialized with a space (line 128). This field displays the reason for the authorization response.

### AUTHCD DFHMDF
This DFHMDF macro defines a display field for the authorization code. It is positioned at line 9, column 68, with a length of 6 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 135). The field is initialized with a space (line 138). This field displays the authorization code generated by the authorization system.

### AUTHAMT DFHMDF
This DFHMDF macro defines a display field for the authorization amount. It is positioned at line 11, column 11, with a length of 12 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 145). The field is initialized with a space (line 148). This field displays the amount authorized for the transaction.

### POSEMD DFHMDF
This DFHMDF macro defines a display field for the POS entry mode. It is positioned at line 11, column 46, with a length of 4 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 155). The field is initialized with a space (line 158). This field displays the mode of entry used at the point of sale.

### AUTHSRC DFHMDF
This DFHMDF macro defines a display field for the authorization source. It is positioned at line 11, column 68, with a length of 10 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 165). The field is initialized with a space (line 168). This field displays the source of the authorization request.

### MCCCD DFHMDF
This DFHMDF macro defines a display field for the Merchant Category Code (MCC). It is positioned at line 13, column 13, with a length of 4 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 175). The field is initialized with a space (line 178). This field displays the MCC code associated with the merchant.

### CRDEXP DFHMDF
This DFHMDF macro defines a display field for the card expiration date. It is positioned at line 13, column 42, with a length of 5 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 185). The field is initialized with a space (line 188). This field displays the expiration date of the card.

### AUTHTYP DFHMDF
This DFHMDF macro defines a display field for the authorization type. It is positioned at line 13, column 64, with a length of 14 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 195). The field is initialized with a space (line 198). This field displays the type of authorization performed.

### TRNID DFHMDF
This DFHMDF macro defines a display field for the transaction ID. It is positioned at line 15, column 12, with a length of 15 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 205). The field is initialized with a space (line 208). This field displays the unique identifier for the transaction.

### AUTHMTC DFHMDF
This DFHMDF macro defines a display field for the match status. It is positioned at line 15, column 46, with a length of 1 character. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to RED (line 215). The field is initialized with a space (line 218). This field displays the status of the match process.

### AUTHFRD DFHMDF
This DFHMDF macro defines a display field for the fraud status. It is positioned at line 15, column 67, with a length of 10 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to RED (line 225). The field is initialized with a space (line 228). This field displays the status of the fraud check.

### Merchant Details (Literal)
This DFHMDF macro defines a static text field displaying the literal 'Merchant Details ------------------------------- -----------------------------'. It is positioned at line 17, column 2, with a length of 76 characters. The color is set to NEUTRAL (line 229). This field serves as a visual separator and heading for the merchant details section.

### MERNAME DFHMDF
This DFHMDF macro defines a display field for the merchant name. It is positioned at line 19, column 9, with a length of 25 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 240). The field is initialized with a space (line 243). This field displays the name of the merchant.

### MERID DFHMDF
This DFHMDF macro defines a display field for the merchant ID. It is positioned at line 19, column 55, with a length of 15 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 250). The field is initialized with a space (line 253). This field displays the unique identifier for the merchant.

### MERCITY DFHMDF
This DFHMDF macro defines a display field for the merchant city. It is positioned at line 21, column 9, with a length of 25 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 260). The field is initialized with a space (line 263). This field displays the city where the merchant is located.

### MERST DFHMDF
This DFHMDF macro defines a display field for the merchant state. It is positioned at line 21, column 49, with a length of 2 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 270). The field is initialized with a space (line 273). This field displays the state where the merchant is located.

### MERZIP DFHMDF
This DFHMDF macro defines a display field for the merchant zip code. It is positioned at line 21, column 61, with a length of 10 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to BLUE (line 280). The field is initialized with a space (line 283). This field displays the zip code of the merchant's location.

### ERRMSG DFHMDF
This DFHMDF macro defines a display field for error messages. It is positioned at line 23, column 1, with a length of 78 characters. The attributes are set to ASKIP, BRT, and FSET, meaning the cursor will skip over this field, it will be displayed with bright intensity, and the field can be modified by the program. The color is set to RED (line 285). This field is used to display error messages to the user.

### Function Key Prompts (Literal)
This DFHMDF macro defines a static text field displaying the function key prompts. It is positioned at line 24, column 1, with a length of 45 characters. The attributes are set to ASKIP and NORM, meaning the cursor will skip over this field, and it will be displayed with normal intensity. The color is set to YELLOW (line 289). The field is initialized with ' F3=Back  F5=Mark/Remove Fraud  F8=Next Auth' (line 292). This field provides guidance to the user on how to navigate the screen and perform actions.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU01 | map | 19 | Artifact 'COPAU01' (map) is never referenced by any other artifact in the dependency graph |
| COPAU1A | screen | 26 | Screen/Map 'COPAU1A' is never sent to or received from by any program |

## Open Questions

- ? What program populates the fields on this screen?
  - Context: The BMS map defines the screen layout, but the source code doesn't indicate which program populates the fields with data.
