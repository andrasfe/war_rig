# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 14:20:07.895624

## Purpose

DDL script that creates the CARDDEMO.AUTHFRDS table to store authorization and fraud-related data for card transactions. The table captures details such as card number, authorization timestamp, response codes, transaction amounts, merchant information, and fraud indicators. Primary key is composite on CARD_NUM and AUTH_TS.

**Business Context**: Supports card payment authorization processing and fraud detection by storing transaction authorization details and fraud status.

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Table storing card authorization fraud data including card details, timestamps, auth codes, amounts, merchant info, and fraud flags |

## Business Rules

- **BR001**: CARD_NUM and AUTH_TS are mandatory fields forming the primary key
