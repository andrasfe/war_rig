# CCPAURLY

**File**: `cpy/CCPAURLY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 02:33:33.455666

## Purpose

This copybook defines the data structure for a Pending Authorization Response record. It specifies fields for card number, transaction ID, authorization ID code, response code, response reason, and approved amount. The structure supports payment authorization processing in mainframe COBOL programs.

**Business Context**: Credit card transaction authorization responses, capturing details from pending authorization requests (e.g., from card issuers)

## Business Rules

- **BR001**: PA-RL-CARD-NUM field stores the card number as a fixed 16-character alphanumeric string
- **BR002**: PA-RL-TRANSACTION-ID field stores the transaction identifier as a fixed 15-character alphanumeric string
- **BR003**: PA-RL-AUTH-ID-CODE field stores the authorization ID code as a fixed 6-character alphanumeric string
- **BR004**: PA-RL-AUTH-RESP-CODE field stores the authorization response code as a fixed 2-character alphanumeric string
- **BR005**: PA-RL-AUTH-RESP-REASON field stores the authorization response reason as a fixed 4-character alphanumeric string
- **BR006**: PA-RL-APPROVED-AMT field stores the approved amount as a signed numeric value with 10 integer digits, decimal point, and 2 fractional digits

## Open Questions

- ? In which specific programs or sections (e.g., WORKING-STORAGE, LINKAGE, FILE_SECTION) is this copybook included?
  - Context: Copybook file does not specify usage context
