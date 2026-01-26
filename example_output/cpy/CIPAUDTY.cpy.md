# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 15:13:23.453341

## Purpose

This copybook defines the data structure for an IMS segment named 'PENDING AUTHORIZATION DETAILS'. It specifies fields for authorization keys, card details, merchant information, transaction amounts, response codes, and status indicators for matching and fraud detection. The structure is used to store and process pending credit card authorization data in a payment processing system.

**Business Context**: Credit card authorization processing, including pending transaction matching, fraud confirmation/removal, and merchant transaction details in an IMS database environment.

## Business Rules

- **BR001**: Authorization response approved when response code is '00'
- **BR002**: Match status conditions for pending authorization: Pending (P), Declined (D), Expired (E), Matched (M)
- **BR003**: Fraud status: Confirmed (F) or Removed (R)
