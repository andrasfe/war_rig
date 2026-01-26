# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 02:32:37.862817

## Purpose

This DDL script creates the CARDDEMO.AUTHFRDS table to store authorization records related to card fraud detection. It defines columns for card details, transaction information, merchant data, and fraud indicators such as AUTH_FRAUD and MATCH_STATUS. A primary key on CARD_NUM and AUTH_TS ensures uniqueness for each authorization event per card.

**Business Context**: Supports fraud analysis and reporting for card authorization transactions by capturing detailed authorization and merchant data.

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Table storing card authorization fraud details including card number, timestamp, transaction amount, merchant info, and fraud flags |

## Business Rules

- **BR001**: Enforces uniqueness of authorization records per card and timestamp to prevent duplicate entries for the same authorization event
