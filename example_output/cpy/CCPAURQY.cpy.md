# CCPAURQY

**File**: `cpy/CCPAURQY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-10 17:19:49.338046

## Purpose

This copybook defines the data structure for a pending authorization request. It includes fields for transaction details such as card number, amount, merchant information, and date/time of authorization. The structure is likely used for communication between systems involved in payment processing.

**Business Context**: Payment authorization processing

## Paragraphs/Procedures

### DATA-DEFINITION
This section defines the data elements required for a pending authorization request. It acts as a template for storing and exchanging authorization information between different modules or systems. The data elements include authorization date and time (PA-RQ-AUTH-DATE, PA-RQ-AUTH-TIME), card details (PA-RQ-CARD-NUM, PA-RQ-CARD-EXPIRY-DATE), transaction specifics (PA-RQ-TRANSACTION-AMT, PA-RQ-TRANSACTION-ID), and merchant details (PA-RQ-MERCHANT-ID, PA-RQ-MERCHANT-NAME, PA-RQ-MERCHANT-CITY, PA-RQ-MERCHANT-STATE, PA-RQ-MERCHANT-ZIP). The message type and source (PA-RQ-MESSAGE-TYPE, PA-RQ-MESSAGE-SOURCE) along with processing code (PA-RQ-PROCESSING-CODE) and acquirer country code (PA-RQ-ACQR-COUNTRY-CODE) provide additional context for the authorization request. The POS entry mode (PA-RQ-POS-ENTRY-MODE) indicates how the transaction was initiated. No specific business logic or error handling is present within this data definition section. This section does not call any other paragraphs or programs; its sole purpose is to define the data structure.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| 05:PA-RQ-AUTH-DATE | column | 1 | Artifact '05:PA-RQ-AUTH-DATE' (column) is never referenced by any other artifact in the dependency graph |

## Open Questions

- ? Where is this copybook used?
  - Context: Cannot determine which programs or modules include this copybook.
