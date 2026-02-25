# CCPAURQY

**File**: `cpy/CCPAURQY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-25 15:31:28.442890

## Purpose

This COBOL copybook defines a data structure at the 05 group level for a 'PENDING AUTHORIZATION REQUEST' used in payment processing systems. It specifies 18 elementary fields including timestamps (auth date/time), card details (number, expiry, type), transaction data (amount, ID, processing code), message metadata (type, source), and merchant information (ID, name, city, state, zip, category code, acquirer country). The structure ensures consistent data formatting via PIC clauses for use in COBOL programs handling credit card authorizations.

**Business Context**: Credit card transaction authorization workflows, capturing pending auth request details for validation, processing, and logging.

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

- ? In which section (WORKING-STORAGE, LINKAGE, FILE-SECTION) is this copybook typically included?
  - Context: Copybook itself does not specify; must be determined from including programs.
