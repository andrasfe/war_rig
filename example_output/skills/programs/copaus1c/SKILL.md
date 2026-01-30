---
name: copaus1c
description: "This program provides a detailed view of a specific pending authorization message within the CardDemo application. It allows users to view transaction details, navigate to the next authorization record, and toggle the fraud status of a transaction."
---

# COPAUS1C

**Type:** COBOL (ONLINE_CICS)
**Context:** CardDemo Authorization Module - enables administrators or investigators to review flagged or pending credit card authorizations for potential fraud.

## Purpose

This program provides a detailed view of a specific pending authorization message within the CardDemo application. It allows users to view transaction details, navigate to the next authorization record, and toggle the fraud status of a transaction.

## Business Rules

- **BR001**: Fraud Status Toggle
- **BR002**: Authorization Response Mapping

## Called Programs

- COPAUS2C (CICS_LINK)
- CDEMO-TO-PROGRAM (STATIC_CALL)

## Inputs

- **DFHCOMMAREA** (CICS_COMMAREA): Contains session data including account ID and the selected authorization key from the summary screen.
- **PAUTSUM0** (IMS_SEGMENT): Pending Authorization Summary root segment, accessed by Account ID.
- **PAUTDTL1** (IMS_SEGMENT): Pending Authorization Details child segment, containing specific transaction data.

## Outputs

- **COPAU1A** (CICS_MAP): The Detail View screen displaying authorization details like card number, amount, merchant info, and fraud status.
- **PAUTDTL1** (IMS_SEGMENT): Updated detail segment when fraud status is toggled.

## Copybooks Used

- **COCOM01Y**: Common Commarea for CardDemo
- **COPAU01**: BMS Mapset for Authorization screens
- **CIPAUSMY**: IMS Segment Layout for Auth Summary
- **CIPAUDTY**: IMS Segment Layout for Auth Details

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAUS1C
- Understand business rules implemented in COPAUS1C
- Trace program calls from COPAUS1C
- Identify inputs/outputs for COPAUS1C
- Maintain or modify COPAUS1C

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.