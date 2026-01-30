---
name: cbpaup0j
description: "This JCL job CBPAUP0J executes the IMS batch message processing (BMP) program CBPAUP0C via the DFSRRC00 region controller to delete expired authorizations. It configures IMS libraries, PSB/DBD libraries, and provides inline SYSIN parameters. Standard IMS batch DD statements are defined for inputs, outputs, and dumps."
---

# CBPAUP0J

**Type:** JCL (BATCH)
**Context:** Manages authorizations in the CARDDEMO system by deleting expired entries from an IMS database using PSBPAUTB.

## Purpose

This JCL job CBPAUP0J executes the IMS batch message processing (BMP) program CBPAUP0C via the DFSRRC00 region controller to delete expired authorizations. It configures IMS libraries, PSB/DBD libraries, and provides inline SYSIN parameters. Standard IMS batch DD statements are defined for inputs, outputs, and dumps.

## Called Programs

- DFSRRC00 (STATIC_CALL)
- CBPAUP0C (DYNAMIC_CALL)

## Inputs

- **SYSIN** (PARAMETER): Inline control parameters '00,00001,00001,Y' passed to DFSRRC00 for IMS program execution (likely PCB/segment/position flags).
- **IMS.PSBLIB** (OTHER): IMS PSB library containing PSBPAUTB definition.
- **IMS.DBDLIB** (OTHER): IMS DBD library for database definitions accessed via PSBPAUTB.

## Outputs

- **SYSPRINT** (REPORT): Standard print output from DFSRRC00 and CBPAUP0C execution.
- **SYSUDUMP** (REPORT): System dump output on program abend.
- **SYSOUT** (REPORT): General job and IMS output listings.

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of CBPAUP0J
- Trace program calls from CBPAUP0J
- Identify inputs/outputs for CBPAUP0J
- Maintain or modify CBPAUP0J

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.