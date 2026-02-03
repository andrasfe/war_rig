# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: COPYBOOK
**Analyzed**: 2026-02-03 21:07:20.078881

## Purpose

This copybook defines the data structure for the IMS segment related to pending authorization details. It includes fields for authorization keys, card details, transaction information, merchant details, match status, and fraud indicators. The copybook is used to store and process information about pending authorizations within an IMS database.

## Paragraphs/Procedures

### N/A
This copybook does not contain any paragraphs. It only defines data structures for use in COBOL programs. The copybook defines the PA-AUTHORIZATION-KEY group item, which contains the PA-AUTH-DATE-9C and PA-AUTH-TIME-9C fields, used to uniquely identify the authorization record. Other fields include PA-AUTH-ORIG-DATE, PA-AUTH-ORIG-TIME, PA-CARD-NUM, PA-AUTH-TYPE, PA-CARD-EXPIRY-DATE, PA-MESSAGE-TYPE, PA-MESSAGE-SOURCE, PA-AUTH-ID-CODE, PA-AUTH-RESP-CODE, PA-AUTH-RESP-REASON, PA-PROCESSING-CODE, PA-TRANSACTION-AMT, PA-APPROVED-AMT, PA-MERCHANT-CATAGORY-CODE, PA-ACQR-COUNTRY-CODE, PA-POS-ENTRY-MODE, PA-MERCHANT-ID, PA-MERCHANT-NAME, PA-MERCHANT-CITY, PA-MERCHANT-STATE, PA-MERCHANT-ZIP, PA-TRANSACTION-ID, PA-MATCH-STATUS, PA-AUTH-FRAUD, PA-FRAUD-RPT-DATE and FILLER. These fields store information related to the authorization transaction, merchant details, and fraud indicators. The 88 level condition names (PA-AUTH-APPROVED, PA-MATCH-PENDING, PA-MATCH-AUTH-DECLINED, PA-MATCH-PENDING-EXPIRED, PA-MATCHED-WITH-TRAN, PA-FRAUD-CONFIRMED, PA-FRAUD-REMOVED) are used to simplify conditional logic when processing the data.
