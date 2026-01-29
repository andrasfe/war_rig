---
name: cbpaup0c
description: "This batch COBOL IMS program deletes expired pending authorization messages from the IMS database. It reads pending authorization summary and detail segments, checks if they are expired based on a configurable expiry period, and deletes the expired detail segments. It also deletes the summary segment if all its detail segments have been deleted. [2, 5]"
---

# CBPAUP0C

**Type:** COBOL (BATCH)
**Context:** CardDemo - Authorization Module [3]

## Purpose

This batch COBOL IMS program deletes expired pending authorization messages from the IMS database. It reads pending authorization summary and detail segments, checks if they are expired based on a configurable expiry period, and deletes the expired detail segments. It also deletes the summary segment if all its detail segments have been deleted. [2, 5]

## Business Rules

- **BR001**: Determine if a pending authorization detail is expired. If the difference between the current date and the authorization date is greater than or equal to the expiry days, the authorization is considered expired.
- **BR002**: Adjust approved/declined authorization counts and amounts based on authorization response code before deleting an expired authorization detail.
- **BR003**: Delete the authorization summary if both approved and declined authorization counts are zero. [156]

## Inputs

- **SYSIN** (PARAMETER): Contains parameters for expiry days, checkpoint frequency, checkpoint display frequency, and debug flag. [98-108, 189]
- **IMS Database - PAUTSUM0 (Pending Authorization Summary)** (IMS_SEGMENT): Contains summary information about pending authorizations. [116, 224]
- **IMS Database - PAUTDTL1 (Pending Authorization Details)** (IMS_SEGMENT): Contains detailed information about pending authorizations. [120, 256]

## Outputs

- **DISPLAY** (REPORT): Displays program start message, parameter values, date, summary counts (read, deleted), and checkpoint messages. [171-178, 190-194, 362-363, 366-368]
- **IMS Database - PAUTSUM0 (Pending Authorization Summary)** (IMS_SEGMENT): Pending Authorization Summary segment is deleted if all detail segments are deleted. [156-158, 335-338]
- **IMS Database - PAUTDTL1 (Pending Authorization Details)** (IMS_SEGMENT): Pending Authorization Detail segment is deleted if it is expired. [149-151, 310-313]
- **RETURN-CODE** (RETURN_CODE): Set to 16 if the program abends. [382]

## Copybooks Used

- **CIPAUSMY**: Defines the layout of the PENDING-AUTH-SUMMARY segment.
- **CIPAUDTY**: Defines the layout of the PENDING-AUTH-DETAILS segment.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of CBPAUP0C
- Understand business rules implemented in CBPAUP0C
- Identify inputs/outputs for CBPAUP0C
- Maintain or modify CBPAUP0C

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.