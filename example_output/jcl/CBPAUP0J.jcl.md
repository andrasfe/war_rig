# CBPAUP0J

**File**: `jcl/CBPAUP0J.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-28 14:55:41.693454

## Purpose

This JCL executes an IMS program (DFSRRC00) to delete expired authorizations, using the BMP region controller. It specifies the program CBPAUP0C and PSB PSBPAUTB for the IMS execution.

**Business Context**: This job likely supports security and compliance by removing outdated access privileges.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| IMS.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB library containing IMS modules. |
| XXXXXXXX.PROD.LOADLIB | IOType.FILE_SEQUENTIAL | Application load library, likely containing CBPAUP0C. |
| IMS.PROCLIB | IOType.FILE_SEQUENTIAL | IMS procedure library. |
| IMS.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSB library containing PSBPAUTB. |
| IMS.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBD library. |
| SYSIN | IOType.FILE_SEQUENTIAL | Control input for the IMS program. Contains parameters '00,00001,00001,Y'. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSOUX | IOType.REPORT | System output. |
| SYSOUT | IOType.REPORT | System output. |
| SYSABOUT | IOType.REPORT | System output for ABEND information. |
| ABENDAID | IOType.REPORT | System output for ABEND aid. |
| SYSPRINT | IOType.REPORT | System print output. |
| SYSUDUMP | IOType.REPORT | System user dump output. |
| IMSERR | IOType.REPORT | IMS error output. |

## Paragraphs/Procedures

### CBPAUP0J
[Citadel] Paragraph identified by static analysis

### STEP01
[Citadel] Paragraph identified by static analysis

## Open Questions

- ? What is the purpose of the SYSIN input parameters '00,00001,00001,Y'?
  - Context: The meaning of these parameters is not clear from the JCL itself.
- ? What is the DSN for XXXXXXXX.PROD.LOADLIB?
  - Context: The DSN is masked with XXXXXXXX.
