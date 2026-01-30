# COPAU01 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** COPAU01
- **File Name:** cpy-bms/COPAU01.cpy
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:44:31.500364

## Purpose

**Summary:** This copybook defines the COBOL data structures for the COPAU01 BMS map used in CICS online transactions. It provides the input area COPAU1AI with length, format/attribute, and data fields for screen elements including transaction names, titles, dates, times, card numbers, authorization details, merchant information, and error messages. The output area COPAU1AO redefines COPAU1AI with BMS attribute fields (C/P/H/V) and output data fields for screen rendering.

**Business Context:** Supports credit card authorization screens in payment processing, displaying and capturing transaction, card, authorization response, and merchant details.
**Program Type:** ONLINE_CICS

## Inputs

### COPAU1AI

- **Type:** CICS_MAP
- **Description:** Input map structure containing length (L), format/attribute (F/A), and input data (I) fields for all screen elements

## Outputs

### COPAU1AO

- **Type:** CICS_MAP
- **Description:** Output map structure redefining input, with changed/protected/highlighted/unprotected attributes (C/P/H/V) and output data (O) fields for screen display

## Paragraphs

### COPAU01

[Citadel] Paragraph identified by static analysis

### COPAU1AI

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Dead Code

- **COPAU1AI** (record_layout): Record layout 'COPAU1AI' is never used by any program
