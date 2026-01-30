---
name: pasflpcb
description: "This COBOL copybook defines the data structure for the PASFL Program Communication Block (PCB) used in IMS DL/I application programs. It specifies fields including the database name, segment level, PCB status, processing options, segment name, key feedback name, number of sensitive segments, and the key feedback buffer. The structure facilitates communication between the application and the IMS PASFL database for segment search and access."
---

# PASFLPCB

**Type:** COPYBOOK (UTILITY)
**Context:** IMS hierarchical database (DL/I) access for the PASFL database

## Purpose

This COBOL copybook defines the data structure for the PASFL Program Communication Block (PCB) used in IMS DL/I application programs. It specifies fields including the database name, segment level, PCB status, processing options, segment name, key feedback name, number of sensitive segments, and the key feedback buffer. The structure facilitates communication between the application and the IMS PASFL database for segment search and access.

## Copybooks Used

- **PASFLPCB**: Defines the IMS PCB structure for accessing the PASFL database, including DBD name, status, segment info, and key feedback

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PASFLPCB
- Maintain or modify PASFLPCB

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.