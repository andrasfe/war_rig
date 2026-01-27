# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 02:41:50.175960

## Purpose

This COBOL copybook defines the data structure for an IMS database segment named 'PENDING AUTHORIZATION DETAILS'. It specifies the layout starting with the group item PA-AUTHORIZATION-KEY (line 19) containing authorization date/time keys, card details, response codes, transaction amounts, merchant information, and status flags. Condition names (88-levels) are provided for key business states such as authorization approval, match status, and fraud indicators.

**Business Context**: Supports payment processing for credit card authorizations, including matching pending transactions, fraud detection, and merchant transaction logging in an IMS database environment.

## Business Rules

- **BR001**: Authorization response is approved when the response code is '00'
- **BR002**: Match status indicates pending, declined, expired, or matched with transaction
- **BR003**: Fraud status is confirmed or removed
