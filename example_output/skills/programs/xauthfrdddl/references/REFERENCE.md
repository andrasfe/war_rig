# XAUTHFRD - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** XAUTHFRD
- **File Name:** ddl/XAUTHFRD.ddl
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:27:52.864503

## Purpose

**Summary:** This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index keys on CARD_NUM in ascending order and AUTH_TS in descending order. The COPY YES option is specified to allow the index to participate in image copy operations.

**Business Context:** None
**Program Type:** UTILITY

## Inputs

### CARDDEMO.AUTHFRDS

- **Type:** DB2_TABLE
- **Description:** DB2 table on which the unique index is created

## Outputs

### CARDDEMO.XAUTHFRD

- **Type:** OTHER
- **Description:** Unique DB2 index created on AUTHFRDS table

## Business Rules

### BR001

**Description:** Enforces uniqueness on the combination of CARD_NUM and AUTH_TS

**Logic:** Unique index prevents insertion of duplicate key values

**Conditions:**
- `CARD_NUM ASC`
- `AUTH_TS DESC`

## Paragraphs

### XAUTHFRD

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## SQL Operations

- **Unknown** on AUTHFRDS

## Dead Code

- **XAUTHFRD** (index): Artifact 'XAUTHFRD' (index) is never referenced by any other artifact in the dependency graph
