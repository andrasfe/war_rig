# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-25 15:33:29.085369

## Purpose

This copybook defines the IMS segment layout for Pending Authorization Details (CIPAUDTY). It structures fields for authorization keys, timestamps, card details, merchant information, transaction amounts, and status flags including approval, match status, and fraud indicators. The structure supports payment processing workflows involving authorization checks and transaction matching.

**Business Context**: IMS database segment for managing pending payment authorizations in a financial or e-commerce transaction processing system

## Business Rules

- **BR001**: Authorization response is approved when the response code is '00'
- **BR002**: Match status indicates pending authorization matching
- **BR003**: Match status indicates authorization declined
- **BR004**: Match status indicates pending authorization expired
- **BR005**: Match status indicates matched with transaction
- **BR006**: Fraud is confirmed on the authorization
- **BR007**: Fraud has been removed from the authorization

## Paragraphs/Procedures

### CIPAUDTY
[Citadel] Paragraph identified by static analysis
