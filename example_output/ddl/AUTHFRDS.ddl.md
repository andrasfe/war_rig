# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 23:05:19.831168

## Purpose

This DDL file defines the CARDDEMO.AUTHFRDS table, which stores authorization and fraud-related data for card transactions. The table includes fields for card number, authorization timestamp, transaction details, merchant information, fraud indicators, and customer identifiers. It establishes a primary key on CARD_NUM and AUTH_TS.

**Business Context**: This table likely supports fraud detection and authorization processes within a card transaction system.

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Table definition for storing card authorization and fraud data. |

## Paragraphs/Procedures

### ~~AUTHFRDS~~ (Dead Code)
*Table 'AUTHFRDS' is never read, written, or referenced by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| AUTHFRDS | table | 1 | Table 'AUTHFRDS' is never read, written, or referenced by any program |
