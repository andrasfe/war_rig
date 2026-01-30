# COPAU00 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** COPAU00
- **File Name:** cpy-bms/COPAU00.cpy
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:40:09.796021

## Purpose

**Summary:** This COBOL copybook defines the BMS (Basic Mapping Support) map structure COPAU0AI for CICS online screens, supporting display and input of customer account information including account ID, name, address, status, phone, approval/decline counts, credit/cash limits and balances, and up to five selectable recent transactions. COPAU0AI provides input fields (marked I) for terminal receive and output preparation, while the redefines COPAU0AO provides output fields (marked O) with BMS attributes (C=changed, P=protected, H=highlight, V=visible). It is used in CICS programs for account inquiry/update screens.

**Business Context:** Customer account profile display and transaction history selection in a financial or credit card processing system.
**Program Type:** ONLINE_CICS

## Inputs

### COPAU0AI

- **Type:** CICS_MAP
- **Description:** Primary BMS map structure for receiving input from CICS terminal, including selection fields SEL0001I to SEL0005I (PIC X(1), lines 150,198,246,294,384), transaction details, account data, and error message ERRMSGI (PIC X(78), line 390)

## Outputs

### COPAU0AO

- **Type:** CICS_MAP
- **Description:** Redefined BMS map structure for sending output to CICS terminal, including account details like ACCTIDO (PIC X(11), line 434), balances like CREDBALO (PIC X(12), line 506), transaction history like TRNID01O (PIC X(16), line 530), and error message ERRMSGO (PIC X(78), line 764)

## Paragraphs

### COPAU00

[Citadel] Paragraph identified by static analysis

### COPAU0AI

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Dead Code

- **COPAU0AI** (record_layout): Record layout 'COPAU0AI' is never used by any program
