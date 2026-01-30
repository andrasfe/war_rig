---
name: copaus0c
description: "COPAUS0C is a CICS online program that displays a paginated list of authorizations for a specified account ID on the PAULST screen. It handles first-time invocation by initializing data and sending the initial screen, subsequent interactions by receiving map data, processing user keys (ENTER for selection/validation, PF3 to exit, PF7/PF8 for paging), and navigating to detail screens via XCTL. Error conditions like invalid account ID or selection are handled by displaying messages without abending."
---

# COPAUS0C

**Type:** COBOL (ONLINE_CICS)
**Context:** Card demo application screen for viewing and selecting payment authorizations associated with an account.

## Purpose

COPAUS0C is a CICS online program that displays a paginated list of authorizations for a specified account ID on the PAULST screen. It handles first-time invocation by initializing data and sending the initial screen, subsequent interactions by receiving map data, processing user keys (ENTER for selection/validation, PF3 to exit, PF7/PF8 for paging), and navigating to detail screens via XCTL. Error conditions like invalid account ID or selection are handled by displaying messages without abending.

## Business Rules

- **BR001**: Account ID must be provided and numeric before fetching authorizations.
- **BR002**: Authorization selection requires 'S' or 's' in one of SEL0001I-SEL0005I fields.
- **BR003**: PF7 only allowed if current page > 1 to prevent paging before first page.

## Called Programs

- CDEMO-TO-PROGRAM (CICS_XCTL)

## Inputs

- **COPAU0AI** (CICS_MAP): Input map fields including ACCTIDI for account ID entry and SEL0001I-SEL0005I for authorization selection flags.
- **CARDDEMO-COMMAREA** (CICS_COMMAREA): CICS commarea carrying persistent data like CDEMO-ACCT-ID, CDEMO-PGM-REENTER flag, page numbers, authorization keys, and navigation targets.
- **EIBAID** (OTHER): CICS attention identifier (DFHENTER, DFHPF3, DFHPF7, DFHPF8) indicating user key press.

## Outputs

- **COPAU0AO** (CICS_MAP): Output map populated with ACCTIDO display field, ERRMSGO for error messages, authorization list details, and cursor positioning via ACCTIDL.
- **CARDDEMO-COMMAREA** (CICS_COMMAREA): Updated commarea with navigation data (CDEMO-TO-PROGRAM, CDEMO-ACCT-ID, selected keys, flags) passed on RETURN or XCTL.

## Copybooks Used

- **COPAU0AI**: Defines input map fields for account ID and selection indicators.
- **COPAU0AO**: Defines output map fields for screen display including messages and list.
- **CARDDEMO-COMMAREA**: Common commarea structure for navigation and data persistence across programs.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of COPAUS0C
- Understand business rules implemented in COPAUS0C
- Trace program calls from COPAUS0C
- Identify inputs/outputs for COPAUS0C
- Maintain or modify COPAUS0C

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.