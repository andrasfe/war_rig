# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-30 19:28:03.606245

## Purpose

This DDL script creates a DB2 table named CARDDEMO.AUTHFRDS to store authorization and fraud-related details for card transactions. The table includes fields for card details, authorization timestamps, merchant information, transaction amounts, and fraud indicators such as AUTH_FRAUD and FRAUD_RPT_DATE. It supports fraud detection and reporting in a card processing environment.

**Business Context**: Card authorization fraud logging and analysis for payment processing systems

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Table storing card authorization details including fraud indicators, merchant data, and transaction information |

## Business Rules

- **BR001**: CARD_NUM and AUTH_TS form a composite primary key to ensure uniqueness of each authorization record per card

## Paragraphs/Procedures

### ~~AUTHFRDS~~ (Dead Code)
*Table 'AUTHFRDS' is never read, written, or referenced by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| AUTHFRDS | table | 1 | Table 'AUTHFRDS' is never read, written, or referenced by any program |

## Open Questions

- ? Which programs or jobs execute this DDL?
  - Context: DDL file does not reference callers; usage depends on deployment scripts
- ? Detailed business validation rules for field populations (e.g., valid values for AUTH_TYPE)?
  - Context: DDL defines structure only, not constraints beyond NULL and PK
