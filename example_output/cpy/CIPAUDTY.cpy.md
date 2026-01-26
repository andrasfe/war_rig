# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 02:31:52.701531

## Purpose

This copybook defines the data structure for an IMS segment capturing PENDING AUTHORIZATION DETAILS, including authorization key, card information, response codes, merchant details, transaction amounts, and fraud/match status flags. It provides level 05 fields starting with PA-AUTHORIZATION-KEY (containing packed date/time) followed by alphanumeric and numeric fields for transaction metadata. Condition names (88 levels) are defined for authorization approval, match statuses, and fraud indicators.

**Business Context**: Supports payment processing workflows for storing and querying pending credit card authorizations, enabling transaction matching, fraud detection, and approval validation in a mainframe IMS database.

## Business Rules

- **BR001**: Authorization response approved if PA-AUTH-RESP-CODE is '00'
- **BR002**: Match status conditions: Pending ('P'), Declined ('D'), Expired ('E'), Matched ('M')
- **BR003**: Fraud status: Confirmed ('F') or Removed ('R')

## Open Questions

- ? In which programs and at what level (e.g., 01 PA-AUTH-SEGMENT COPY CIPAUDTY) is this copybook included?
  - Context: Copybook starts at level 05, implying it is nested within a higher-level segment definition; usage context not provided in file.
