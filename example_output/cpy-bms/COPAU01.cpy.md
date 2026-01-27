# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 23:06:00.393695

## Purpose

This copybook defines the input and output data structures for a CICS BMS map, COPAU1A. It contains field definitions for both input (COPAU1AI) and output (COPAU1AO) screen layouts, including attributes, lengths, and data types for various fields such as transaction name, titles, dates, times, card number, authorization details, and merchant information.

**Business Context**: This copybook is used in a CICS environment to define the structure of data displayed on and received from a user screen, likely related to payment authorization processing.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Defines the input fields received from the CICS BMS map COPAU1A. Includes fields for transaction name (TRNNAMEI), titles (TITLE01I, TITLE02I), current date and time (CURDATEI, CURTIMEI), card number (CARDNUMI), authorization details (AUTHDTI, AUTHTMI, AUTHRSPI, AUTHRSNI, AUTHCDI, AUTHAMTI), POS entry mode (POSEMDI), authorization source (AUTHSRCI), MCC code (MCCCDI), card expiry (CRDEXPI), authorization type (AUTHTYPI), transaction ID (TRNIDI), authorization match (AUTHMTCI), authorization fraud (AUTHFRDI), merchant name (MERNAMEI), merchant ID (MERIDI), merchant city (MERCITYI), merchant state (MERSTI), merchant zip (MERZIPI), and error message (ERRMSGI). |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Defines the output fields sent to the CICS BMS map COPAU1A.  It redefines COPAU1AI and includes control characters (C), protected attributes (P), highlight attributes (H), modified data tags (V), and output data fields (O) for each input field. These attributes control how the data is displayed on the screen. |

## Paragraphs/Procedures

### COPAU01
[Citadel] Paragraph identified by static analysis

### ~~COPAU1AI~~ (Dead Code)
*Record layout 'COPAU1AI' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU1AI | record_layout | 1 | Record layout 'COPAU1AI' is never used by any program |
