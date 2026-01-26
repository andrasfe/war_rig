# CCPAURLY

**File**: `cpy/CCPAURLY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 17:41:57.439186

## Purpose

This COBOL copybook defines a level-05 group structure named PA-RL for a Pending Authorization Response record. It specifies fields for card number, transaction ID, authorization ID code, authorization response code, response reason, and approved amount. The structure standardizes data exchange or storage in payment authorization processes.

**Business Context**: Payment card transaction processing, specifically capturing responses from authorization requests in systems handling pending authorizations (e.g., Amazon payment systems).

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PA-RL | IOType.OTHER | Pending Authorization Response structure defining card number (16 chars), transaction ID (15 chars), auth ID code (6 chars), auth response code (2 chars), auth response reason (4 chars), and approved amount (numeric 10.2). |

## Business Rules

- **BR001**: PA-RL-CARD-NUM field holds the card number and must be exactly 16 alphanumeric characters.
- **BR002**: PA-RL-TRANSACTION-ID field holds the transaction ID and must be exactly 15 alphanumeric characters.
- **BR003**: PA-RL-AUTH-ID-CODE field holds the authorization ID code and must be exactly 6 alphanumeric characters.
- **BR004**: PA-RL-AUTH-RESP-CODE field holds the authorization response code and must be exactly 2 alphanumeric characters.
- **BR005**: PA-RL-AUTH-RESP-REASON field holds the authorization response reason and must be exactly 4 alphanumeric characters.
- **BR006**: PA-RL-APPROVED-AMT field holds the approved amount as a numeric value with 10 digits before the decimal and 2 after.

## Open Questions

- ? In which programs, sections (e.g., WORKING-STORAGE, LINKAGE, FILE-SECTION), and for what I/O (e.g., VSAM file, CICS COMMAREA) is CCPAURLY.cpy included?
  - Context: Copybook file contains only data definitions, no usage context provided.
