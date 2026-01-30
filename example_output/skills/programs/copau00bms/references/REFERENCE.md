# COPAU00 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** COPAU00
- **File Name:** COPAU00.bms
- **File Type:** BMS
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:40:27.088006

## Purpose

**Summary:** This BMS mapset defines the Pending Authorization Screen (COPAU0A) for the CardDemo application. It provides a user interface to search pending authorizations by Account ID, display customer details including name, address, status, phone, limits, balances, approval/decline counts, and a scrollable list of up to 5 transactions with fields for selection, transaction ID, date, time, type, A/D, status, and amount. Users interact via input fields and PF keys for navigation.

**Business Context:** Serves credit card authorization processing by allowing operators to view, select, and navigate pending authorization transactions for an account.
**Program Type:** ONLINE_CICS

## Inputs

### ACCTID

- **Type:** CICS_MAP
- **Description:** Unprotected input field for entering Account ID to search for pending authorizations

### SEL0001

- **Type:** CICS_MAP
- **Description:** Unprotected selection field (Sel) for first transaction in list

### SEL0002

- **Type:** CICS_MAP
- **Description:** Unprotected selection field (Sel) for second transaction in list

### SEL0003

- **Type:** CICS_MAP
- **Description:** Unprotected selection field (Sel) for third transaction in list

### SEL0004

- **Type:** CICS_MAP
- **Description:** Unprotected selection field (Sel) for fourth transaction in list

### SEL0005

- **Type:** CICS_MAP
- **Description:** Unprotected selection field (Sel) for fifth transaction in list

## Outputs

### COPAU0A

- **Type:** CICS_MAP
- **Description:** Primary map displaying screen header (Tran, Date, Prog, Time), title, search prompt, customer info (name, ID, address, status, phone), limits/balances/amounts (credit/cash lim/bal, appr/decl amt), transaction list headers and data rows, instructions, error message, and PF key help

### ERRMSG

- **Type:** CICS_MAP
- **Description:** Error message display field with bright and red attributes

## Business Rules

### BR001

**Description:** User must type 'S' in one of the SEL fields to select and view details of a specific authorization from the transaction list

**Logic:** Instruction explicitly displayed on screen to guide user interaction

**Conditions:**
- `User enters 'S' in SEL0001-SEL0005`

### BR002

**Description:** Screen navigation uses PF keys: ENTER to continue, F3 to go back, F7 for backward scroll, F8 for forward scroll

**Logic:** PF key help displayed at bottom of screen

**Conditions:**
- `PF3 pressed`
- `PF7 pressed`
- `PF8 pressed`

## Paragraphs

### COPAU00

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

### COPAU0A

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Error Handling

- **Application errors:** Display message in ERRMSG field (bright, red, FSET)

## CICS Operations

- SEND MAP
- RECEIVE MAP

## Dead Code

- **COPAU00** (map): Artifact 'COPAU00' (map) is never referenced by any other artifact in the dependency graph
- **COPAU0A** (screen): Screen/Map 'COPAU0A' is never sent to or received from by any program
