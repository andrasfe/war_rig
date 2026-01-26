# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 15:10:54.921567

## Purpose

DDL script that creates the AUTHFRDS table in the CARDDEMO schema. The table stores detailed authorization transaction data for card processing, including card numbers, timestamps, merchant details, transaction amounts, response codes, and fraud indicators. It supports fraud detection and reporting by maintaining historical authorization records with uniqueness enforced on card number and timestamp.

**Business Context**: Card authorization processing and fraud analysis in a demonstration (CARDDEMO) system

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Table storing authorization fraud details including card, merchant, transaction, and fraud fields |

## Business Rules

- **BR001**: CARD_NUM and AUTH_TS form a composite primary key to ensure each authorization per card is uniquely identified by timestamp
- **BR002**: CARD_NUM and AUTH_TS are mandatory fields for all records

## Open Questions

- ? Specific database system (e.g., DB2 version or dialect)
  - Context: Data types like TIMESTAMP, DECIMAL(12,2), SMALLINT, VARCHAR(22) suggest DB2 or similar but not explicitly stated
- ? Usage of nullable fields and business validation beyond DDL constraints
  - Context: Many fields lack NOT NULL (e.g., lines 4-27 except PK), unclear if application enforces further rules
