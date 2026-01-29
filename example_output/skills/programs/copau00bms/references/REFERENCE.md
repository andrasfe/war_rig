# COPAU00 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** COPAU00
- **File Name:** COPAU00.bms
- **File Type:** BMS
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:54:49.526333

## Purpose

**Summary:** This BMS map defines the screen layout for the CardDemo Pending Authorization screen. It displays customer account information and a list of pending transactions, allowing the user to select a transaction for further details.

**Business Context:** This screen is used to view and manage pending authorizations for credit card transactions.
**Program Type:** ONLINE_CICS

## Inputs

### ACCTID

- **Type:** CICS_MAP
- **Description:** Account ID entered by the user to search for pending authorizations.

### SEL0001 - SEL0005

- **Type:** CICS_MAP
- **Description:** Selection fields for each transaction displayed on the screen. User enters a value to select a specific transaction.

## Outputs

### COPAU0A

- **Type:** CICS_MAP
- **Description:** The entire screen layout, including labels, data fields, and messages. Displays customer information and pending transactions.

### TRNNAME

- **Type:** CICS_MAP
- **Description:** Transaction Name

### TITLE01

- **Type:** CICS_MAP
- **Description:** Title 1

### CURDATE

- **Type:** CICS_MAP
- **Description:** Current Date

### PGMNAME

- **Type:** CICS_MAP
- **Description:** Program Name

### TITLE02

- **Type:** CICS_MAP
- **Description:** Title 2

### CURTIME

- **Type:** CICS_MAP
- **Description:** Current Time

### CNAME

- **Type:** CICS_MAP
- **Description:** Customer Name

### CUSTID

- **Type:** CICS_MAP
- **Description:** Customer ID

### ADDR001

- **Type:** CICS_MAP
- **Description:** Address Line 1

### ACCSTAT

- **Type:** CICS_MAP
- **Description:** Account Status

### ADDR002

- **Type:** CICS_MAP
- **Description:** Address Line 2

### PHONE1

- **Type:** CICS_MAP
- **Description:** Phone Number

### APPRCNT

- **Type:** CICS_MAP
- **Description:** Approval Count

### DECLCNT

- **Type:** CICS_MAP
- **Description:** Decline Count

### CREDLIM

- **Type:** CICS_MAP
- **Description:** Credit Limit

### CASHLIM

- **Type:** CICS_MAP
- **Description:** Cash Limit

### APPRAMT

- **Type:** CICS_MAP
- **Description:** Approved Amount

### CREDBAL

- **Type:** CICS_MAP
- **Description:** Credit Balance

### CASHBAL

- **Type:** CICS_MAP
- **Description:** Cash Balance

### DECLAMT

- **Type:** CICS_MAP
- **Description:** Declined Amount

### TRNID01 - TRNID05

- **Type:** CICS_MAP
- **Description:** Transaction ID for each of the 5 transactions displayed.

### PDATE01 - PDATE05

- **Type:** CICS_MAP
- **Description:** Transaction Date for each of the 5 transactions displayed.

### PTIME01 - PTIME05

- **Type:** CICS_MAP
- **Description:** Transaction Time for each of the 5 transactions displayed.

### PTYPE01 - PTYPE05

- **Type:** CICS_MAP
- **Description:** Transaction Type for each of the 5 transactions displayed.

### PAPRV01 - PAPRV05

- **Type:** CICS_MAP
- **Description:** Transaction Approval indicator for each of the 5 transactions displayed.

### PSTAT01 - PSTAT05

- **Type:** CICS_MAP
- **Description:** Transaction Status for each of the 5 transactions displayed.

### PAMT001 - PAMT005

- **Type:** CICS_MAP
- **Description:** Transaction Amount for each of the 5 transactions displayed.

### ERRMSG

- **Type:** CICS_MAP
- **Description:** Error message field to display error conditions to the user.

## Paragraphs

### COPAU0A

**(Dead Code)**

This paragraph defines the overall structure and attributes of the CICS map COPAU0A, which represents the Pending Authorization screen. It specifies the screen size as 24 lines by 80 columns (line 28). It sets various control options, including enabling the alarm, freeing the keyboard after input, enabling extended attributes, specifying COBOL as the programming language, setting the mode to INOUT for both input and output, using automatic storage, and enabling the TIOAPFX option (lines 19-24). The TYPE=&&SYSPARM indicates that the map type (e.g., MAP or DSECT) is determined by a system parameter at compile time (line 25). This paragraph essentially acts as the container for all the individual fields and labels that make up the screen, defining the overall environment in which they will be displayed. It doesn't directly process data or implement business logic but sets the stage for the interaction between the user and the application.

## Data Flow

### Reads From

- **ACCTID:** ACCTID
- **SEL0001:** SEL0001
- **SEL0002:** SEL0002
- **SEL0003:** SEL0003
- **SEL0004:** SEL0004
- **SEL0005:** SEL0005

### Writes To

- **COPAU0A:** TRNNAME, TITLE01, CURDATE, PGMNAME, TITLE02, CURTIME, CNAME, CUSTID, ADDR001, ACCSTAT, ADDR002, PHONE1, APPRCNT, DECLCNT, CREDLIM, CASHLIM, APPRAMT, CREDBAL, CASHBAL, DECLAMT, TRNID01, PDATE01, PTIME01, PTYPE01, PAPRV01, PSTAT01, PAMT001, TRNID02, PDATE02, PTIME02, PTYPE02, PAPRV02, PSTAT02, PAMT002, TRNID03, PDATE03, PTIME03, PTYPE03, PAPRV03, PSTAT03, PAMT003, TRNID04, PDATE04, PTIME04, PTYPE04, PAPRV04, PSTAT04, PAMT004, TRNID05, PDATE05, PTIME05, PTYPE05, PAPRV05, PSTAT05, PAMT005, ERRMSG

## Error Handling

- **Error condition:** Display error message in ERRMSG field

## CICS Operations

- SEND MAP
- RECEIVE MAP

## Dead Code

- **COPAU00** (map): Artifact 'COPAU00' (map) is never referenced by any other artifact in the dependency graph
- **COPAU0A** (screen): Screen/Map 'COPAU0A' is never sent to or received from by any program
