# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 17:36:58.109388

## Purpose

DDL script that creates the AUTHFRDS table in the CARDDEMO schema. The table stores authorization transaction details including card number, timestamp, merchant information, transaction amounts, and fraud indicators for fraud analysis. Primary key is composite on CARD_NUM and AUTH_TS.

**Business Context**: Fraud detection and reporting for card authorization transactions in a demonstration (CARDDEMO) environment.

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Table storing authorization fraud details including card number (line 2), auth timestamp (line 3), transaction amount (line 12), fraud flag (line 24), and merchant details (lines 17-21). |

## Business Rules

- **BR001**: CARD_NUM and AUTH_TS form the primary key ensuring unique identification of each authorization record.
- **BR002**: CARD_NUM is mandatory for all records.
- **BR003**: AUTH_TS is mandatory for all records.

## Open Questions

- ? Detailed business validation rules for populating fields like AUTH_FRAUD or MATCH_STATUS.
  - Context: DDL defines structure only; no logic for field population or validation provided.
- ? Usage context of fields like MESSAGE_TYPE, AUTH_ID_CODE, or PROCESSING_CODE.
  - Context: Field names suggest ISO 8583 or payment network standards, but exact mappings not specified.
