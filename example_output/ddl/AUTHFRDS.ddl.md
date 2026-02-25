# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-25 15:24:35.289459

## Purpose

DDL script that creates the AUTHFRDS table in the CARDDEMO schema to store authorization transaction details for fraud detection and analysis in card processing. Columns capture card details, timestamps, merchant information, transaction amounts, response codes, and fraud indicators. Primary key is composite on CARD_NUM and AUTH_TS.

**Business Context**: Card authorization fraud monitoring and reporting system

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Table storing authorization fraud details including card number, auth timestamp, transaction amounts, merchant data, and fraud flags |

## Paragraphs/Procedures

### ~~AUTHFRDS~~ (Dead Code)
*Table 'AUTHFRDS' is never read, written, or referenced by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| AUTHFRDS | table | 1 | Table 'AUTHFRDS' is never read, written, or referenced by any program |
