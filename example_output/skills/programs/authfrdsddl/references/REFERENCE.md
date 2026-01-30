# AUTHFRDS - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** AUTHFRDS
- **File Name:** ddl/AUTHFRDS.ddl
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:28:03.606245

## Purpose

**Summary:** This DDL script creates a DB2 table named CARDDEMO.AUTHFRDS to store authorization and fraud-related details for card transactions. The table includes fields for card details, authorization timestamps, merchant information, transaction amounts, and fraud indicators such as AUTH_FRAUD and FRAUD_RPT_DATE. It supports fraud detection and reporting in a card processing environment.

**Business Context:** Card authorization fraud logging and analysis for payment processing systems
**Program Type:** UTILITY

## Outputs

### CARDDEMO.AUTHFRDS

- **Type:** DB2_TABLE
- **Description:** Table storing card authorization details including fraud indicators, merchant data, and transaction information

## Business Rules

### BR001

**Description:** CARD_NUM and AUTH_TS form a composite primary key to ensure uniqueness of each authorization record per card

**Logic:** Enforced by PRIMARY KEY constraint in DDL

**Conditions:**
- `CARD_NUM NOT NULL`
- `AUTH_TS NOT NULL`

## Paragraphs

### AUTHFRDS

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## SQL Operations

- **Unknown** on CARDDEMO.AUTHFRDS

## Dead Code

- **AUTHFRDS** (table): Table 'AUTHFRDS' is never read, written, or referenced by any program
