---
name: copaus2c
description: The COPAUS2C program is a CICS COBOL program that marks an authorization message as fraudulent by inserting or updating a record in the CARDDEMO.AUTHFRDS table. It receives transaction details via the CICS COMMAREA and uses this data to populate the AUTHFRDS table with information about the potentially fraudulent transaction.
---

# COPAUS2C

**Type:** COBOL (ONLINE_CICS)
**Context:** This program is part of an authorization module within the CardDemo application, likely used to flag suspicious transactions for further investigation and prevent future fraudulent activities. 

## Purpose

The COPAUS2C program is a CICS COBOL program that marks an authorization message as fraudulent by inserting or updating a record in the CARDDEMO.AUTHFRDS table. It receives transaction details via the CICS COMMAREA and uses this data to populate the AUTHFRDS table with information about the potentially fraudulent transaction.

## Business Rules

- **BR001**: If a record with the same CARD_NUM and AUTH_TS already exists in the AUTHFRDS table, update the AUTH_FRAUD and FRAUD_RPT_DATE columns. Otherwise, insert a new record.

## Inputs

- **DFHCOMMAREA** (CICS_COMMAREA): Contains transaction details including account ID (WS-ACCT-ID), customer ID (WS-CUST-ID), and fraud report data (WS-FRAUD-AUTH-RECORD) based on CIPAUDTY copybook, and fraud status record (WS-FRAUD-STATUS-RECORD).

## Copybooks Used

- **DFHBMSCA**: Defines BMS control blocks (commented out)
- **SQLCA**: SQL Communication Area for DB2 interaction
- **AUTHFRDS**: Defines the structure of the AUTHFRDS table
- **CIPAUDTY**: Defines the structure of the fraud report data

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAUS2C
- Understand business rules implemented in COPAUS2C
- Identify inputs/outputs for COPAUS2C
- Maintain or modify COPAUS2C

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.