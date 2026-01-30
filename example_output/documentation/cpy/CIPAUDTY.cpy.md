# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-30 19:47:38.794636

## Purpose

This copybook defines the data structure for an IMS segment named 'PENDING AUTHORIZATION DETAILS'. It specifies fields for authorization keys, dates, times, card details, merchant information, transaction amounts, response codes, and status indicators for matching and fraud detection. The structure supports payment authorization processing in a financial transaction system.

**Business Context**: Serves payment authorization workflows, storing pending auth details for transaction matching, fraud monitoring, and response tracking in an IMS database within Amazon's financial systems.

## Business Rules

- **BR001**: Authorization response is approved when the response code is '00'
- **BR002**: Match status can be Pending ('P'), Declined ('D'), Expired ('E'), or Matched ('M')
- **BR003**: Fraud status is Confirmed ('F') or Removed ('R')

## Paragraphs/Procedures

### CIPAUDTY
[Citadel] Paragraph identified by static analysis
