---
name: pautbpcb
description: "This copybook defines the data structure PAUTBPCB (line 17), which is the Program Communication Block (PCB) for the PAUT IMS database. It includes fields for database name (PAUT-DBDNAME, line 18), current segment level (PAUT-SEG-LEVEL, line 19), PCB status code (PAUT-PCB-STATUS, line 20), processing options (PAUT-PCB-PROCOPT, line 21), segment name (PAUT-SEG-NAME, line 23), key feedback name offset (PAUT-KEYFB-NAME, line 24), number of sensitive segments (PAUT-NUM-SENSEGS, line 25), and a 255-byte key feedback buffer (PAUT-KEYFB, line 26). This structure is used by IMS DL/I programs to manage database calls and retrieve status and positioning information."
---

# PAUTBPCB

**Type:** COPYBOOK (UTILITY)
**Context:** IMS DL/I hierarchical database navigation and access for the PAUT database

## Purpose

This copybook defines the data structure PAUTBPCB (line 17), which is the Program Communication Block (PCB) for the PAUT IMS database. It includes fields for database name (PAUT-DBDNAME, line 18), current segment level (PAUT-SEG-LEVEL, line 19), PCB status code (PAUT-PCB-STATUS, line 20), processing options (PAUT-PCB-PROCOPT, line 21), segment name (PAUT-SEG-NAME, line 23), key feedback name offset (PAUT-KEYFB-NAME, line 24), number of sensitive segments (PAUT-NUM-SENSEGS, line 25), and a 255-byte key feedback buffer (PAUT-KEYFB, line 26). This structure is used by IMS DL/I programs to manage database calls and retrieve status and positioning information.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PAUTBPCB
- Maintain or modify PAUTBPCB

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.