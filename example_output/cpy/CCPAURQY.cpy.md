# CCPAURQY

**File**: `cpy/CCPAURQY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 02:43:41.819054

## Purpose

This copybook defines the record structure for a Pending Authorization Request used in payment processing systems. It specifies fields for authorization date/time, card details, transaction amount, merchant information, and other ISO-8583-like elements essential for auth request handling. The structure ensures consistent data layout across programs that include this copybook.

**Business Context**: Credit card transaction authorization workflow, capturing pending auth requests from acquirers or POS terminals for fraud/risk assessment.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| N/A | IOType.OTHER | Not applicable; this is a copybook defining data structures, not an executable program with inputs. Fields like PA-RQ-AUTH-DATE (line 19), PA-RQ-CARD-NUM (line 21) serve as record layouts for input files or commareas in including programs. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| N/A | IOType.OTHER | Not applicable; copybook defines reusable structures, not program outputs. Fields like PA-RQ-TRANSACTION-ID (line 36) can be populated for output records in using programs. |

## Business Rules

- **BR001**: PA-RQ-AUTH-DATE must conform to 6-character alphanumeric format
- **BR002**: PA-RQ-CARD-NUM is fixed 16-character field for card number
- **BR003**: PA-RQ-TRANSACTION-AMT uses signed decimal format with 10 integer digits and 2 decimal places

## Open Questions

- ? Which programs include or reference this CCPAURQY copybook?
  - Context: No including programs or usage context provided in the file
