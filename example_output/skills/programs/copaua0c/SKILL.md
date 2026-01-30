---
name: copaua0c
description: "COPAUA0C is a CICS COBOL program that processes payment authorization requests triggered via IBM MQ. It initializes by retrieving MQ trigger message data from CICS, configures and opens the request queue using MQOPEN, and sets up for reading and processing messages in a loop. The program orchestrates initialization, main processing, and termination before returning to CICS."
---

# COPAUA0C

**Type:** COBOL (ONLINE_CICS)
**Context:** Payment card authorization processing, involving queue-based request handling, account validation, and response generation

## Purpose

COPAUA0C is a CICS COBOL program that processes payment authorization requests triggered via IBM MQ. It initializes by retrieving MQ trigger message data from CICS, configures and opens the request queue using MQOPEN, and sets up for reading and processing messages in a loop. The program orchestrates initialization, main processing, and termination before returning to CICS.

## Business Rules

- **BR001**: Only use retrieved MQTM data if CICS RETRIEVE response is normal
- **BR002**: Set request queue open flag only if MQOPEN compcode is OK, otherwise treat as critical error

## Called Programs

- MQOPEN (STATIC_CALL)

## Inputs

- **MQTM** (CICS_COMMAREA): CICS input containing MQ trigger message with queue name and trigger data
- **REQUEST-QUEUE** (OTHER): IBM MQ queue named in WS-REQUEST-QNAME for reading authorization requests

## Outputs

- **ERROR-LOG** (OTHER): Error details logged via 9500-LOG-ERROR on MQOPEN failure

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAUA0C
- Understand business rules implemented in COPAUA0C
- Trace program calls from COPAUA0C
- Identify inputs/outputs for COPAUA0C
- Maintain or modify COPAUA0C

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.