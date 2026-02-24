# CCPAURQY

**File**: `cpy/CCPAURQY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-24 04:02:00.986110

## Purpose

This COBOL copybook defines a level 05 data group structure for a Pending Authorization Request, including fields for auth date/time (lines 19-20), card number/expiry/auth type (lines 21-23), message types/source (lines 24-25), processing code (line 26), transaction amount (line 27), merchant category/acquirer country/POS mode (lines 28-30), and merchant details/transaction ID (lines 31-36). It standardizes data for credit card authorization requests in payment processing. Header comments confirm the purpose and Apache License (lines 1-18).

**Business Context**: Credit card transaction authorization processing, capturing cardholder, merchant, and transaction details for pending approvals.

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

## Open Questions

- ? In which programs and sections is this copybook copied?
  - Context: Copybook provides only data definitions; no usage information present.
