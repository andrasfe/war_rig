# CCPAURQY

**File**: `cpy/CCPAURQY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 14:25:59.153146

## Purpose

This COBOL copybook defines a level-05 group structure named implicitly under PA-RQ-* fields for a Pending Authorization Request record. It specifies 18 elementary data items including authorization date/time (lines 19-20), card number and expiry (21,23), transaction details (26-27,36), merchant category and ID (28,31), and location data (32-35). The structure standardizes data for payment authorization queries in mainframe applications.

**Business Context**: Credit card or payment authorization processing, capturing request details from POS/merchant for pending auth queries.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PENDING-AUTH-REQUEST | IOType.OTHER | Data structure for pending authorization request record, defining fields such as PA-RQ-AUTH-DATE PIC X(06), PA-RQ-AUTH-TIME PIC X(06), PA-RQ-CARD-NUM PIC X(16), PA-RQ-AUTH-TYPE PIC X(04), PA-RQ-CARD-EXPIRY-DATE PIC X(04), PA-RQ-MESSAGE-TYPE PIC X(06), PA-RQ-MESSAGE-SOURCE PIC X(06), PA-RQ-PROCESSING-CODE PIC 9(06), PA-RQ-TRANSACTION-AMT PIC +9(10).99, PA-RQ-MERCHANT-CATAGORY-CODE PIC X(04), PA-RQ-ACQR-COUNTRY-CODE PIC X(03), PA-RQ-POS-ENTRY-MODE PIC 9(02), PA-RQ-MERCHANT-ID PIC X(15), PA-RQ-MERCHANT-NAME PIC X(22), PA-RQ-MERCHANT-CITY PIC X(13), PA-RQ-MERCHANT-STATE PIC X(02), PA-RQ-MERCHANT-ZIP PIC X(09), PA-RQ-TRANSACTION-ID PIC X(15). |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PENDING-AUTH-REQUEST | IOType.OTHER | Data structure for pending authorization request record (same 18 fields as input structure, lines 19-36), potentially used symmetrically for output in programs copying this structure for request/response handling in payment contexts. |
