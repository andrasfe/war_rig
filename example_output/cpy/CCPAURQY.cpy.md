# CCPAURQY

**File**: `cpy/CCPAURQY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 17:39:12.182544

## Purpose

This COBOL copybook defines the data structure for a Pending Authorization Request record used in payment processing systems. It includes fields for authorization date and time, card number, expiry date, transaction details, merchant information, and other related data elements. The structure supports capturing and passing authorization request data between programs.

**Business Context**: Serves credit card or payment authorization workflows, holding details like card info, transaction amount, merchant data, and processing codes for pending auth requests (citation: lines 19-37).

## Open Questions

- ? In which specific programs is this copybook included?
  - Context: Copybook analysis does not reveal including programs; must review referencing COBOL source files.
- ? Detailed field formats and validation rules (e.g., packed decimal usage, edit masks)?
  - Context: PIC clauses provided but no usage or validation logic in copybook itself.
