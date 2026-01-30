# CIPAUDTY - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** CIPAUDTY
- **File Name:** cpy/CIPAUDTY.cpy
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:47:38.794636

## Purpose

**Summary:** This copybook defines the data structure for an IMS segment named 'PENDING AUTHORIZATION DETAILS'. It specifies fields for authorization keys, dates, times, card details, merchant information, transaction amounts, response codes, and status indicators for matching and fraud detection. The structure supports payment authorization processing in a financial transaction system.

**Business Context:** Serves payment authorization workflows, storing pending auth details for transaction matching, fraud monitoring, and response tracking in an IMS database within Amazon's financial systems.
**Program Type:** UTILITY

## Business Rules

### BR001

**Description:** Authorization response is approved when the response code is '00'

**Logic:** Defined via 88-level condition name PA-AUTH-APPROVED

**Conditions:**
- `PA-AUTH-RESP-CODE = '00'`

### BR002

**Description:** Match status can be Pending ('P'), Declined ('D'), Expired ('E'), or Matched ('M')

**Logic:** 88-level conditions on PA-MATCH-STATUS field

**Conditions:**
- `PA-MATCH-STATUS = 'P'`
- `PA-MATCH-STATUS = 'D'`
- `PA-MATCH-STATUS = 'E'`
- `PA-MATCH-STATUS = 'M'`

### BR003

**Description:** Fraud status is Confirmed ('F') or Removed ('R')

**Logic:** 88-level conditions on PA-AUTH-FRAUD field

**Conditions:**
- `PA-AUTH-FRAUD = 'F'`
- `PA-AUTH-FRAUD = 'R'`

## Paragraphs

### CIPAUDTY

[Citadel] Paragraph identified by static analysis

## Data Flow
