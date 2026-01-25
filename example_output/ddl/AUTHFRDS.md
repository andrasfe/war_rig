# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-25 18:31:02.877811

## Purpose

DDL script creates the CARDDEMO.AUTHFRDS table to store authorization transaction details for fraud analysis, including card number, timestamp, merchant information, transaction amounts, and fraud indicators. Fields cover authorization response, processing codes, and links to accounts/customers. Primary key ensures uniqueness by CARD_NUM and AUTH_TS.

**Business Context**: Fraud detection and reporting for credit/debit card authorization transactions

## Inputs

| Name | Type | Description |
|------|------|-------------|
| N/A | IOType.OTHER | DDL script defines table structure; no runtime inputs |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Table storing auth fraud data created by this DDL |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| N/A | CallType.OTHER | DDL script; no called programs |

## Business Rules

- **BR001**: CARD_NUM is mandatory
- **BR002**: AUTH_TS is mandatory
- **BR003**: Composite primary key on CARD_NUM and AUTH_TS ensures unique authorizations per card and timestamp

## Paragraphs/Procedures

### N/A
DDL script; no paragraphs or procedures

## Open Questions

- ? Which COBOL/PL/I programs read/write this table?
  - Context: Cannot determine from DDL alone; requires cross-referencing source code using this table
- ? Detailed business validation rules enforced by applications using this table?
  - Context: DDL shows only DB constraints; application logic unknown
