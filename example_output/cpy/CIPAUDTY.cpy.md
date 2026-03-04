# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-04 04:43:59.741681

## Purpose

This copybook defines the data structure for the IMS segment containing pending authorization details. It includes fields for authorization keys, card details, transaction information, merchant details, and fraud-related information.

## Paragraphs/Procedures

### CIPAUDTY
This copybook defines the data structure for the IMS segment related to pending authorization details. It is used to store information about transactions awaiting authorization, including authorization keys (PA-AUTHORIZATION-KEY) composed of date (PA-AUTH-DATE-9C) and time (PA-AUTH-TIME-9C), original authorization date and time (PA-AUTH-ORIG-DATE, PA-AUTH-ORIG-TIME), card number (PA-CARD-NUM), authorization type (PA-AUTH-TYPE), card expiry date (PA-CARD-EXPIRY-DATE), message type (PA-MESSAGE-TYPE), message source (PA-MESSAGE-SOURCE), authorization ID code (PA-AUTH-ID-CODE), authorization response code (PA-AUTH-RESP-CODE) with condition PA-AUTH-APPROVED, authorization response reason (PA-AUTH-RESP-REASON), processing code (PA-PROCESSING-CODE), transaction amount (PA-TRANSACTION-AMT), approved amount (PA-APPROVED-AMT), merchant category code (PA-MERCHANT-CATAGORY-CODE), acquirer country code (PA-ACQR-COUNTRY-CODE), POS entry mode (PA-POS-ENTRY-MODE), merchant ID (PA-MERCHANT-ID), merchant name (PA-MERCHANT-NAME), merchant city (PA-MERCHANT-CITY), merchant state (PA-MERCHANT-STATE), merchant zip code (PA-MERCHANT-ZIP), transaction ID (PA-TRANSACTION-ID), match status (PA-MATCH-STATUS) with conditions PA-MATCH-PENDING, PA-MATCH-AUTH-DECLINED, PA-MATCH-PENDING-EXPIRED, PA-MATCHED-WITH-TRAN, authorization fraud (PA-AUTH-FRAUD) with conditions PA-FRAUD-CONFIRMED, PA-FRAUD-REMOVED and fraud report date (PA-FRAUD-RPT-DATE). The copybook provides a structured way to access and manipulate pending authorization data within COBOL programs.
