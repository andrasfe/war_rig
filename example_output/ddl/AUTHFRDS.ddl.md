# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-03-03 16:50:32.277008

## Purpose

This DDL file defines the CARDDEMO.AUTHFRDS table, which stores authorization and fraud-related data for card transactions. The table includes fields for card number, authorization timestamp, transaction amounts, merchant information, fraud indicators, and customer/account identifiers.

**Business Context**: This table likely supports fraud detection and analysis within a card transaction processing system.

## Paragraphs/Procedures

### ~~AUTHFRDS~~ (Dead Code)
*Table 'AUTHFRDS' is never read, written, or referenced by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| AUTHFRDS | table | 1 | Table 'AUTHFRDS' is never read, written, or referenced by any program |

## Open Questions

- ? The specific purpose of each field could be further clarified with additional business context.
  - Context: The DDL only defines the table structure, not the intended usage of each field within the application.
