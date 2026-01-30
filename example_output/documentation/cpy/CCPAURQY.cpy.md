# CCPAURQY

**File**: `cpy/CCPAURQY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-30 19:48:06.106215

## Purpose

This COBOL copybook defines a level 05 group structure for Pending Authorization Request data (PA-RQ-* fields), to be subordinated under a level 01 record in including programs. It includes fields for authorization timestamp, card details, transaction amount, merchant information, and transaction ID, supporting credit card processing workflows. No executable logic is present; it serves solely as a data layout definition.

**Business Context**: Payment authorization processing for card transactions, capturing details from POS or acquirer requests (inferred from field names like PA-RQ-CARD-NUM, PA-RQ-TRANSACTION-AMT, PA-RQ-MERCHANT-ID on lines 21,27,31).

## Paragraphs/Procedures

### CCPAURQY
[Citadel] Paragraph identified by static analysis

### ~~05:PA-RQ-AUTH-DATE~~ (Dead Code)
*Artifact '05:PA-RQ-AUTH-DATE' (column) is never referenced by any other artifact in the dependency graph*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| 05:PA-RQ-AUTH-DATE | column | 1 | Artifact '05:PA-RQ-AUTH-DATE' (column) is never referenced by any other artifact in the dependency graph |
