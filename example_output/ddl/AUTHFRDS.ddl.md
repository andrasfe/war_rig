# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-04-21 13:47:52.613266

## Purpose

This DDL file defines the CARDDEMO.AUTHFRDS table, which stores authorization and fraud-related data for card transactions. The table includes fields for card number, authorization timestamp, transaction details, merchant information, fraud indicators, and customer/account identifiers.

**Business Context**: This table likely supports fraud detection and transaction analysis within a card processing system.

## Paragraphs/Procedures

### CREATE TABLE CARDDEMO.AUTHFRDS
This paragraph defines the structure and constraints for the CARDDEMO.AUTHFRDS table. It specifies the data types, lengths, and nullability of each column, including CARD_NUM, AUTH_TS, AUTH_TYPE, CARD_EXPIRY_DATE, MESSAGE_TYPE, MESSAGE_SOURCE, AUTH_ID_CODE, AUTH_RESP_CODE, AUTH_RESP_REASON, PROCESSING_CODE, TRANSACTION_AMT, APPROVED_AMT, MERCHANT_CATAGORY_CODE, ACQR_COUNTRY_CODE, POS_ENTRY_MODE, MERCHANT_ID, MERCHANT_NAME, MERCHANT_CITY, MERCHANT_STATE, MERCHANT_ZIP, TRANSACTION_ID, MATCH_STATUS, AUTH_FRAUD, FRAUD_RPT_DATE, ACCT_ID, and CUST_ID. The paragraph also defines a primary key constraint on the CARD_NUM and AUTH_TS columns, ensuring uniqueness for each card authorization record. No specific error handling is defined within the DDL itself, as error handling would be managed by the database system during table creation or data manipulation. This DDL statement does not call any other programs or paragraphs; it is a standalone definition.
