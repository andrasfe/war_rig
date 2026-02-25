# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-25 15:29:26.909131

## Purpose

This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index is defined on CARD_NUM in ascending order and AUTH_TS in descending order. The COPY YES clause enables image copy support for the index.

**Business Context**: Supports efficient querying of authorization fraud records (AUTHFRDS) by card number with most recent authorization timestamps first, likely for fraud detection or audit trails.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | DB2 table on which the unique index is created, with columns CARD_NUM and AUTH_TS used as the index key. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.XAUTHFRD | IOType.OTHER | Unique DB2 index enforcing uniqueness on (CARD_NUM ASC, AUTH_TS DESC) with COPY YES option. |

## Business Rules

- **BR001**: Enforce uniqueness on the composite key (CARD_NUM, AUTH_TS) to prevent duplicate authorization fraud records per card and timestamp.

## Paragraphs/Procedures

### CREATE-UNIQUE-INDEX
This paragraph constitutes the entire DDL script and its primary purpose is to define and create a unique index object named CARDDEMO.XAUTHFRD in the CARDDEMO schema. It consumes the existing table CARDDEMO.AUTHFRDS as input, referencing it explicitly on line 2 and its columns CARD_NUM and AUTH_TS on line 3. It produces the index as output, which is declared on line 1 and configured with the COPY YES option on line 4. The business logic enforces data integrity by requiring uniqueness on the composite key CARD_NUM (ascending) and AUTH_TS (descending), optimizing for queries retrieving the latest authorization per card. There is no conditional branching, validation, or error handling explicitly coded in the DDL statement, as DDL execution errors are handled by the DB2 engine. No subordinate paragraphs or external programs are called, as this is a standalone CREATE INDEX statement. The descending order on AUTH_TS ensures the most recent records appear first in index scans.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| XAUTHFRD | index | 1 | Artifact 'XAUTHFRD' (index) is never referenced by any other artifact in the dependency graph |
