---
name: padflpcb
description: "This copybook defines the PADFLPCB data structure, which represents the Program Communication Block (PCB) for the PADFL database in an IMS (Information Management System) environment. It includes standard IMS PCB fields such as the database name (PADFL-DBDNAME), current segment level (PADFL-SEG-LEVEL), PCB status (PADFL-PCB-STATUS), processing options (PADFL-PCB-PROCOPT), segment name (PADFL-SEG-NAME), key feedback length (PADFL-KEYFB-NAME), number of sensitive segments (PADFL-NUM-SENSEGS), and the key feedback buffer (PADFL-KEYFB). This structure is referenced in the LINKAGE SECTION of IMS DL/I programs to facilitate database navigation, segment retrieval, and status checking during GU, GN, GNP, and other IMS calls."
---

# PADFLPCB

**Type:** COPYBOOK (UTILITY)

## Purpose

This copybook defines the PADFLPCB data structure, which represents the Program Communication Block (PCB) for the PADFL database in an IMS (Information Management System) environment. It includes standard IMS PCB fields such as the database name (PADFL-DBDNAME), current segment level (PADFL-SEG-LEVEL), PCB status (PADFL-PCB-STATUS), processing options (PADFL-PCB-PROCOPT), segment name (PADFL-SEG-NAME), key feedback length (PADFL-KEYFB-NAME), number of sensitive segments (PADFL-NUM-SENSEGS), and the key feedback buffer (PADFL-KEYFB). This structure is referenced in the LINKAGE SECTION of IMS DL/I programs to facilitate database navigation, segment retrieval, and status checking during GU, GN, GNP, and other IMS calls.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of PADFLPCB
- Maintain or modify PADFLPCB

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.