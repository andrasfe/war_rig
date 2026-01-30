---
name: cipaudty
description: "This copybook defines the data structure for an IMS segment named 'PENDING AUTHORIZATION DETAILS'. It specifies fields for authorization keys, dates, times, card details, merchant information, transaction amounts, response codes, and status indicators for matching and fraud detection. The structure supports payment authorization processing in a financial transaction system."
---

# CIPAUDTY

**Type:** COPYBOOK (UTILITY)
**Context:** Serves payment authorization workflows, storing pending auth details for transaction matching, fraud monitoring, and response tracking in an IMS database within Amazon's financial systems.

## Purpose

This copybook defines the data structure for an IMS segment named 'PENDING AUTHORIZATION DETAILS'. It specifies fields for authorization keys, dates, times, card details, merchant information, transaction amounts, response codes, and status indicators for matching and fraud detection. The structure supports payment authorization processing in a financial transaction system.

## Business Rules

- **BR001**: Authorization response is approved when the response code is '00'
- **BR002**: Match status can be Pending ('P'), Declined ('D'), Expired ('E'), or Matched ('M')
- **BR003**: Fraud status is Confirmed ('F') or Removed ('R')

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of CIPAUDTY
- Understand business rules implemented in CIPAUDTY
- Maintain or modify CIPAUDTY

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.