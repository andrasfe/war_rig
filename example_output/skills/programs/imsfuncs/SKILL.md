---
name: imsfuncs
description: "This COBOL copybook defines the 01-level group FUNC-CODES containing multiple 05-level elementary items, each a PIC X(04) field initialized with specific 4-character values representing IMS database function codes (e.g., 'GU  ', 'GHU ', 'GN  ', 'REPL', 'ISRT', 'DLET'). It also defines PARMCOUNT as PIC S9(05) VALUE +4 COMP-5, likely specifying the parameter count for IMS calls. These structures are used in programs performing IMS database operations such as Get Unique (GU), Get Hold Unique (GHU), Get Next (GN), Replace (REPL), Insert (ISRT), and Delete (DLET)."
---

# IMSFUNCS

**Type:** COPYBOOK (UTILITY)
**Context:** IMS hierarchical database call parameters

## Purpose

This COBOL copybook defines the 01-level group FUNC-CODES containing multiple 05-level elementary items, each a PIC X(04) field initialized with specific 4-character values representing IMS database function codes (e.g., 'GU  ', 'GHU ', 'GN  ', 'REPL', 'ISRT', 'DLET'). It also defines PARMCOUNT as PIC S9(05) VALUE +4 COMP-5, likely specifying the parameter count for IMS calls. These structures are used in programs performing IMS database operations such as Get Unique (GU), Get Hold Unique (GHU), Get Next (GN), Replace (REPL), Insert (ISRT), and Delete (DLET).

## Copybooks Used

- **IMSFUNCS**: Defines FUNC-CODES group for IMS function codes and PARMCOUNT for parameter count in IMS database calls

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of IMSFUNCS
- Maintain or modify IMSFUNCS

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.