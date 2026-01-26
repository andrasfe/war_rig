# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 17:39:14.388008

## Purpose

This COBOL copybook defines the data structure for an IMS segment named 'PENDING AUTHORIZATION DETAILS'. It provides fields for authorization keys, card information, merchant details, transaction amounts, response codes, and status flags for matching and fraud detection in payment processing. The structure is used to store pending authorization data for further transaction matching and review.

**Business Context**: Credit card authorization processing, transaction matching, and fraud management in a financial transaction system.

## Business Rules

- **BR-PA001**: Authorization response is approved when the response code is '00'
- **BR-PA002**: Match status indicates pending matching
- **BR-PA003**: Match status indicates authorization declined
- **BR-PA004**: Match status indicates pending expired
- **BR-PA005**: Match status indicates matched with transaction
- **BR-PA006**: Fraud is confirmed on the authorization
- **BR-PA007**: Fraud has been removed from the authorization

## Open Questions

- ? Which programs include and use this copybook?
  - Context: No referencing programs are provided in the source file
- ? What is the exact IMS database and segment name using this structure?
  - Context: Comment indicates IMS segment but no explicit database or full segment name beyond description
