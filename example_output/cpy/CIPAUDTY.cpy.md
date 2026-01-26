# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 14:25:46.497608

## Purpose

This copybook defines the data structure for the IMS segment 'PENDING AUTHORIZATION DETAILS'. It specifies the layout starting with PA-AUTHORIZATION-KEY, including fields for authorization dates/times, card number, expiry date, auth type, response codes, transaction amounts, merchant details, transaction ID, match status, and fraud indicators. The structure supports payment authorization processing, transaction matching, and fraud management.

**Business Context**: Payment card transaction authorization, pending match processing, and fraud detection in an IMS database environment.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PA-AUTHORIZATION-KEY | IOType.IMS_SEGMENT | IMS segment structure defining pending authorization details: PA-AUTH-DATE-9C (20), PA-AUTH-TIME-9C (21), PA-AUTH-ORIG-DATE (22), PA-AUTH-ORIG-TIME (23), PA-CARD-NUM (24), PA-AUTH-TYPE (25), PA-CARD-EXPIRY-DATE (26), PA-MESSAGE-TYPE (27), PA-MESSAGE-SOURCE (28), PA-AUTH-ID-CODE (29), PA-AUTH-RESP-CODE (30) with 88 PA-AUTH-APPROVED (31), PA-AUTH-RESP-REASON (32), PA-PROCESSING-CODE (33), PA-TRANSACTION-AMT (34), PA-APPROVED-AMT (35), PA-MERCHANT-CATAGORY-CODE (36), PA-ACQR-COUNTRY-CODE (37), PA-POS-ENTRY-MODE (38), PA-MERCHANT-ID (39), PA-MERCHANT-NAME (40), PA-MERCHANT-CITY (41), PA-MERCHANT-STATE (42), PA-MERCHANT-ZIP (43), PA-TRANSACTION-ID (44), PA-MATCH-STATUS (45) with 88s PA-MATCH-PENDING (46), PA-MATCH-AUTH-DECLINED (47), PA-MATCH-PENDING-EXPIRED (48), PA-MATCHED-WITH-TRAN (49), PA-AUTH-FRAUD (50) with 88s PA-FRAUD-CONFIRMED (51), PA-FRAUD-REMOVED (52), PA-FRAUD-RPT-DATE (53), FILLER (54). Used for reading pending auth data from IMS database. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PA-AUTHORIZATION-KEY | IOType.IMS_SEGMENT | IMS segment structure for writing/updating pending authorization details, same fields as input (lines 19-54). Used for IMS database inserts or updates in authorization processing. |

## Business Rules

- **BR001**: Authorization is approved when response code is '00'
- **BR002**: Match status indicates pending, declined, expired, or matched with transaction
- **BR003**: Fraud status is confirmed or removed
