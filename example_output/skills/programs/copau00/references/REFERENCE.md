# COPAU00 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** COPAU00
- **File Name:** COPAU00.cpy
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:55:24.290509

## Purpose

**Summary:** This copybook defines the data structure COPAU0AI, which appears to be used for screen mapping in a CICS environment. It contains fields for transaction names, titles, dates, times, account IDs, customer information, addresses, phone numbers, and error messages. It also redefines COPAU0AI as COPAU0AO, providing an alternate layout for the same data.

**Business Context:** UNKNOWN
**Program Type:** UNKNOWN

## Inputs

### COPAU0AI

- **Type:** CICS_MAP
- **Description:** Data structure for screen input/output, containing fields for transaction name, titles, current date and time, account ID, customer name, customer ID, address, account status, phone number, and amounts.

## Outputs

### COPAU0AI

- **Type:** CICS_MAP
- **Description:** Data structure for screen input/output, containing fields for transaction name, titles, current date and time, account ID, customer name, customer ID, address, account status, phone number, and amounts.

## Paragraphs

### COPAU00

[Citadel] Paragraph identified by static analysis

### COPAU0AI

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Dead Code

- **COPAU0AI** (record_layout): Record layout 'COPAU0AI' is never used by any program
