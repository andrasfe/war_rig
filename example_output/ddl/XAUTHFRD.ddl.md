# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-04-21 13:47:38.994005

## Purpose

This DDL file creates a unique index named XAUTHFRD on the AUTHFRDS table in the CARDDEMO schema. The index is defined on the CARD_NUM and AUTH_TS columns, sorted in ascending and descending order respectively. The COPY YES clause indicates that the index data should be copied during the index creation process.

## Paragraphs/Procedures

### XAUTHFRD
This DDL script defines a unique index named XAUTHFRD for the CARDDEMO.AUTHFRDS table. The primary purpose of this index is to improve the performance of queries that filter or sort data based on the CARD_NUM and AUTH_TS columns. The index is created with a unique constraint, ensuring that no two rows in the AUTHFRDS table have the same combination of CARD_NUM and AUTH_TS values. The CARD_NUM column is indexed in ascending order (ASC), while the AUTH_TS column is indexed in descending order (DESC). The 'COPY YES' clause specifies that the index data should be copied during the index creation process, potentially improving the speed of index creation at the expense of additional storage space. No specific error handling is defined within this DDL script, as errors during index creation would typically be handled by the database management system.
