# CCPAURQY - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** CCPAURQY
- **File Name:** cpy/CCPAURQY.cpy
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:48:06.106215

## Purpose

**Summary:** This COBOL copybook defines a level 05 group structure for Pending Authorization Request data (PA-RQ-* fields), to be subordinated under a level 01 record in including programs. It includes fields for authorization timestamp, card details, transaction amount, merchant information, and transaction ID, supporting credit card processing workflows. No executable logic is present; it serves solely as a data layout definition.

**Business Context:** Payment authorization processing for card transactions, capturing details from POS or acquirer requests (inferred from field names like PA-RQ-CARD-NUM, PA-RQ-TRANSACTION-AMT, PA-RQ-MERCHANT-ID on lines 21,27,31).
**Program Type:** UTILITY

## Paragraphs

### CCPAURQY

[Citadel] Paragraph identified by static analysis

### 05:PA-RQ-AUTH-DATE

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Dead Code

- **05:PA-RQ-AUTH-DATE** (column): Artifact '05:PA-RQ-AUTH-DATE' (column) is never referenced by any other artifact in the dependency graph
