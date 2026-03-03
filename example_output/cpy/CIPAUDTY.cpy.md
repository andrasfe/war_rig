# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-03 16:50:05.062799

## Purpose

This copybook defines the data structure for the IMS segment containing pending authorization details. It includes fields for authorization keys, card numbers, transaction amounts, merchant information, and fraud indicators related to pending authorizations.

**Business Context**: This data structure is used in the context of processing and managing pending card authorizations, likely within a financial transaction processing system.

## Paragraphs/Procedures

### CIPAUDTY
[Citadel] Paragraph identified by static analysis

## Open Questions

- ? The FILLER field at line 54 is not documented. What is its purpose?
  - Context: The purpose of the filler field is unclear from the code.
