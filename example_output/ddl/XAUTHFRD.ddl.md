# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-03-16 20:02:36.974425

## Purpose

This DDL file creates a unique index named XAUTHFRD on the AUTHFRDS table in the CARDDEMO schema. The index is defined on the CARD_NUM and AUTH_TS columns, sorted in ascending and descending order respectively. The COPY YES clause indicates that the index data should be copied.

## Paragraphs/Procedures

### XAUTHFRD
This DDL script defines a unique index named XAUTHFRD for the CARDDEMO.AUTHFRDS table. The primary purpose of this index is to improve the performance of queries that filter or sort data based on the CARD_NUM and AUTH_TS columns. The index is created with CARD_NUM in ascending order and AUTH_TS in descending order, which suggests that queries often involve sorting by timestamp within a specific card number. The 'COPY YES' clause indicates that the index data should be copied, potentially for backup or recovery purposes. No specific error handling is defined within this DDL script, as index creation failures are typically handled by the database management system. This script does not call any other programs or paragraphs; it is a standalone DDL statement.
