# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-24 03:59:24.682591

## Purpose

DDL script that creates the CARDDEMO.AUTHFRDS DB2 table for storing authorization transaction details related to fraud detection. The table captures card number, timestamp, merchant data, transaction amounts, and fraud indicators. It enforces data integrity via NOT NULL constraints and a composite primary key.

**Business Context**: Supports card fraud analysis by storing historical authorization records with fraud status, merchant details, and transaction metadata for reporting and matching.

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Table storing authorization fraud details including card num, auth timestamp, transaction amounts, merchant info, and fraud flags |

## Business Rules

- **BR001**: CARD_NUM column must not be null
- **BR002**: AUTH_TS column must not be null
- **BR003**: Composite primary key on (CARD_NUM, AUTH_TS)

## Paragraphs/Procedures

### ~~AUTHFRDS~~ (Dead Code)
*Table 'AUTHFRDS' is never read, written, or referenced by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| AUTHFRDS | table | 1 | Table 'AUTHFRDS' is never read, written, or referenced by any program |
