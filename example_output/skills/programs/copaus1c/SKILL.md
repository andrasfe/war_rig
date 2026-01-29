---
name: copaus1c
description: "COPAUS1C is a CICS COBOL program that displays detailed information about an authorization message. It retrieves authorization details from an IMS database, allows users to mark authorizations as fraudulent, and links to another program (COPAUS2C) to handle fraud updates."
---

# COPAUS1C

**Type:** COBOL (ONLINE_CICS)
**Context:** This program is part of a card authorization demo application, providing a detailed view of authorization messages for review and potential fraud flagging.

## Purpose

COPAUS1C is a CICS COBOL program that displays detailed information about an authorization message. It retrieves authorization details from an IMS database, allows users to mark authorizations as fraudulent, and links to another program (COPAUS2C) to handle fraud updates.

## Business Rules

- **BR001**: Authorization response codes are translated to 'A' (Approved) or 'D' (Declined) for display.
- **BR002**: Decline reason codes are translated to a user-friendly description using a table lookup.

## Called Programs

- COPAUS0C (CICS_XCTL)
- COPAUS2C (CICS_LINK)

## Inputs

- **CARDDEMO-COMMAREA** (CICS_COMMAREA): Communication area passed between CICS programs, containing account ID, authorization keys, and other relevant data.
- **PENDING-AUTH-SUMMARY** (IMS_SEGMENT): IMS segment containing summary information about a pending authorization.
- **PENDING-AUTH-DETAILS** (IMS_SEGMENT): IMS segment containing detailed information about a pending authorization.

## Outputs

- **COPAU1A** (CICS_MAP): BMS map displaying authorization details to the user.
- **WS-FRAUD-DATA** (CICS_COMMAREA): Communication area passed to COPAUS2C containing fraud related data.

## Copybooks Used

- **COCOM01Y**: Defines the CARDDEMO-COMMAREA structure for inter-program communication.
- **COPAU01**: Defines the BMS map structure for the authorization view screen.
- **COTTL01Y**: Defines screen titles.
- **CSDAT01Y**: Defines current date variables.
- **CSMSG01Y**: Defines common messages.
- **CSMSG02Y**: Defines abend variables.
- **CIPAUSMY**: Defines the PENDING-AUTH-SUMMARY IMS segment layout.
- **CIPAUDTY**: Defines the PENDING-AUTH-DETAILS IMS segment layout.
- **DFHAID**: Defines CICS AID keys.
- **DFHBMSCA**: Defines BMS communication area.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAUS1C
- Understand business rules implemented in COPAUS1C
- Trace program calls from COPAUS1C
- Identify inputs/outputs for COPAUS1C
- Maintain or modify COPAUS1C

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.