# AUTHFRDS

**File**: `ddl/AUTHFRDS.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-03-16 20:02:37.382161

## Purpose

This DDL file defines the CARDDEMO.AUTHFRDS table, which stores authorization and fraud-related data for card transactions. The table includes fields for card number, authorization timestamp, transaction amounts, merchant details, and fraud indicators.

**Business Context**: This table likely supports fraud detection and analysis within a card transaction processing system.

## Paragraphs/Procedures

### CREATE TABLE CARDDEMO.AUTHFRDS
This paragraph defines the structure and constraints for the CARDDEMO.AUTHFRDS table. It specifies the data types, lengths, and nullability of each column, including CARD_NUM, AUTH_TS, AUTH_TYPE, CARD_EXPIRY_DATE, and others. The table includes transaction details such as amounts, merchant information, and fraud indicators. The primary key is defined as a composite key consisting of CARD_NUM and AUTH_TS, ensuring uniqueness of records based on card number and authorization timestamp. This paragraph does not consume any external inputs directly but defines the table schema based on predefined data types and lengths. It outputs the table definition to the database management system. No specific business logic or error handling is implemented within this DDL statement, as it primarily focuses on defining the table structure. No other programs or paragraphs are called within this DDL statement.
