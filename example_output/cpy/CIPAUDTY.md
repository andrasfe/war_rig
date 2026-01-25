# CIPAUDTY

**File**: `cpy/CIPAUDTY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-25 18:36:28.016446

## Purpose

Defines the data structure for the IMS segment PA-AUTHORIZATION-KEY used to store pending authorization details. Includes fields for authorization timestamps, card details, response codes, transaction amounts, merchant information, match status, and fraud indicators. This structure supports processing of pending payment authorizations in an IMS database.

**Business Context**: Manages pending payment card authorizations, including matching transactions, fraud reporting, and merchant validation in a financial processing system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PA-AUTHORIZATION-KEY | IOType.IMS_SEGMENT | IMS segment structure containing pending authorization details: auth date/time (20-21), orig date/time (22-23), card num/expiry (24,26), auth/message types (25,27-29), response codes/reasons (30,32), amounts (34-35), merchant details (36-43), transaction ID (44), match status (45), fraud flags (50-53). |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PA-AUTHORIZATION-KEY | IOType.IMS_SEGMENT | IMS segment structure for storing/retrieving pending authorization details: auth date/time (20-21), orig date/time (22-23), card num/expiry (24,26), auth/message types (25,27-29), response codes/reasons (30,32), amounts (34-35), merchant details (36-43), transaction ID (44), match status (45), fraud flags (50-53). |

## Business Rules

- **BR001**: Authorization approved status
- **BR002**: Match status is pending
- **BR003**: Match status indicates auth declined
- **BR004**: Match status pending expired
- **BR005**: Match status matched with transaction
- **BR006**: Fraud confirmed status
- **BR007**: Fraud removed status

## Open Questions

- ? Which COBOL programs include this copybook via COPY statement?
  - Context: Copybook source does not reference including programs
- ? In which section (WORKING-STORAGE, FILE-SECTION, etc.) is this copybook typically included?
  - Context: Usage context not specified in copybook
