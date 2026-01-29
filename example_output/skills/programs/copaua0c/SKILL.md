---
name: copaua0c
description: "This COBOL program processes authorization requests, retrieves account and customer information, makes authorization decisions based on available credit and transaction amount, and sends a response. It reads card details, retrieves account and customer data from CICS files and IMS database, and updates the pending authorization summary in IMS. The program also logs errors and warnings to an error log."
---

# COPAUA0C

**Type:** COBOL (ONLINE_CICS)
**Context:** This program is likely part of a credit card authorization system, responsible for validating transactions and updating account information.

## Purpose

This COBOL program processes authorization requests, retrieves account and customer information, makes authorization decisions based on available credit and transaction amount, and sends a response. It reads card details, retrieves account and customer data from CICS files and IMS database, and updates the pending authorization summary in IMS. The program also logs errors and warnings to an error log.

## Business Rules

- **BR001**: Decline authorization if the transaction amount exceeds the available credit.
- **BR002**: If no authorization summary is found, use account data to determine available credit.

## Called Programs

- MQGET (STATIC_CALL)
- MQPUT1 (STATIC_CALL)

## Inputs

- **W01-GET-BUFFER** (CICS_QUEUE): Contains the authorization request message received from the MQ queue.
- **WS-CCXREF-FILE** (FILE_VSAM): Card Cross-Reference file, used to retrieve account and customer IDs based on the card number.
- **WS-ACCTFILENAME** (FILE_VSAM): Account master file, used to retrieve account details such as credit limit and current balance.
- **WS-CUSTFILENAME** (FILE_VSAM): Customer master file, used to retrieve customer details.
- **PENDING-AUTH-SUMMARY** (IMS_SEGMENT): IMS segment PAUTSUM0 containing pending authorization summary data.
- *(+2 more inputs)*

## Outputs

- **W02-PUT-BUFFER** (CICS_QUEUE): Contains the authorization response message sent to the MQ queue.
- **PENDING-AUTH-SUMMARY** (IMS_SEGMENT): Updated pending authorization summary segment in IMS.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAUA0C
- Understand business rules implemented in COPAUA0C
- Trace program calls from COPAUA0C
- Identify inputs/outputs for COPAUA0C
- Maintain or modify COPAUA0C

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.