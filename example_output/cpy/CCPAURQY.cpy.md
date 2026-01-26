# CCPAURQY

**File**: `cpy/CCPAURQY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 02:35:25.452534

## Purpose

This COBOL copybook defines a level-05 data structure for a Pending Authorization Request record used in payment processing applications. It specifies 18 fields capturing details such as authorization date/time, card number, expiry date, transaction amount, merchant information, and transaction ID. The structure supports handling of pending credit card authorization requests from acquirers or POS systems.

**Business Context**: Credit card transaction authorization processing, including merchant and cardholder details for pending requests

## Open Questions

- ? In which programs or files is this copybook included via COPY statement?
  - Context: The copybook itself does not indicate usage; this requires analysis of including source files
