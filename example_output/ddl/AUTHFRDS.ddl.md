# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 02:40:00.295283

## Purpose

This DDL script creates a DB2 table named CARDDEMO.AUTHFRDS to store detailed authorization transaction records for credit/debit card processing. The table includes fields for card details, transaction amounts, merchant information, response codes, and fraud indicators to support fraud detection and analysis. A composite primary key on CARD_NUM and AUTH_TS ensures uniqueness for each authorization event.

**Business Context**: Card payment authorization fraud monitoring and reporting system

## Business Rules

- **BR001**: CARD_NUM and AUTH_TS form the composite primary key, enforcing uniqueness

## Open Questions

- ? Which programs or jobs execute this DDL?
  - Context: No calling context or execution details provided in the file
- ? What are the foreign key relationships or indexes beyond the primary key?
  - Context: Only primary key defined; no additional constraints or indexes specified
