# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-04 03:32:36.979238

## Purpose

This copybook defines the data structure for the IMS segment containing pending authorization details. It includes fields related to authorization keys, card details, transaction information, merchant details, and fraud indicators.

## Paragraphs/Procedures

### CIPAUDTY
This copybook defines the data structure for the IMS segment related to pending authorization details. It starts by defining the PA-AUTHORIZATION-KEY, which includes the PA-AUTH-DATE-9C and PA-AUTH-TIME-9C, representing the date and time of the authorization. It also contains fields like PA-AUTH-ORIG-DATE and PA-AUTH-ORIG-TIME, storing the original authorization date and time. The copybook includes card-related information such as PA-CARD-NUM and PA-CARD-EXPIRY-DATE, as well as transaction details like PA-MESSAGE-TYPE, PA-MESSAGE-SOURCE, PA-AUTH-ID-CODE, PA-AUTH-RESP-CODE, PA-AUTH-RESP-REASON, PA-PROCESSING-CODE, PA-TRANSACTION-AMT, and PA-APPROVED-AMT. Merchant details, including PA-MERCHANT-CATAGORY-CODE, PA-ACQR-COUNTRY-CODE, PA-POS-ENTRY-MODE, PA-MERCHANT-ID, PA-MERCHANT-NAME, PA-MERCHANT-CITY, PA-MERCHANT-STATE, and PA-MERCHANT-ZIP, are also defined. Finally, it includes fields for PA-TRANSACTION-ID, PA-MATCH-STATUS, PA-AUTH-FRAUD, and PA-FRAUD-RPT-DATE, which are used for tracking transaction status, fraud indicators, and reporting dates. The FILLER field at the end is used for padding.
