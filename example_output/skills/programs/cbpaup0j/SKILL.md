---
name: cbpaup0j
description: "This JCL executes an IMS program (DFSRRC00) to delete expired authorizations, using the BMP region controller. It specifies the program CBPAUP0C and PSB PSBPAUTB for the IMS execution."
---

# CBPAUP0J

**Type:** JCL (BATCH)
**Context:** This job likely supports security and compliance by removing outdated access privileges.

## Purpose

This JCL executes an IMS program (DFSRRC00) to delete expired authorizations, using the BMP region controller. It specifies the program CBPAUP0C and PSB PSBPAUTB for the IMS execution.

## Inputs

- **IMS.SDFSRESL** (FILE_SEQUENTIAL): IMS RESLIB library containing IMS modules.
- **XXXXXXXX.PROD.LOADLIB** (FILE_SEQUENTIAL): Application load library, likely containing CBPAUP0C.
- **IMS.PROCLIB** (FILE_SEQUENTIAL): IMS procedure library.
- **IMS.PSBLIB** (FILE_SEQUENTIAL): IMS PSB library containing PSBPAUTB.
- **IMS.DBDLIB** (FILE_SEQUENTIAL): IMS DBD library.
- *(+1 more inputs)*

## Outputs

- **SYSOUX** (REPORT): System output.
- **SYSOUT** (REPORT): System output.
- **SYSABOUT** (REPORT): System output for ABEND information.
- **ABENDAID** (REPORT): System output for ABEND aid.
- **SYSPRINT** (REPORT): System print output.
- *(+2 more outputs)*

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of CBPAUP0J
- Identify inputs/outputs for CBPAUP0J
- Maintain or modify CBPAUP0J

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.