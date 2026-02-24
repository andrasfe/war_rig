# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-24 04:02:01.839964

## Purpose

This COPYBOOK defines the data layout for the IMS database segment 'PENDING AUTHORIZATION DETAILS' (line 2). It includes a group item PA-AUTHORIZATION-KEY at level 05 containing only two packed decimal fields: PA-AUTH-DATE-9C (line 20) and PA-AUTH-TIME-9C (line 21); subsequent level 05 fields such as PA-AUTH-ORIG-DATE (line 22), PA-CARD-NUM (line 24), merchant details (lines 39-43), and status flags with 88-level conditions (lines 31-32, 46-52) are parallel to it, not nested underneath. The structure supports storage of authorization transaction data including card details, response codes, amounts, and fraud indicators.

**Business Context**: Used in payment processing systems for storing pending credit card authorization details in an IMS database, facilitating transaction matching, approval validation, merchant tracking, and fraud detection.

## Paragraphs/Procedures

### CIPAUDTY
[Citadel] Paragraph identified by static analysis
