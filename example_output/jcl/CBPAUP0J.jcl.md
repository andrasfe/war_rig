# CBPAUP0J

**File**: `jcl/CBPAUP0J.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-03-16 20:03:14.264919

## Purpose

This JCL executes an IMS program (DFSRRC00) to delete expired authorizations using a BMP (Batch Message Processing) region. It specifies the program CBPAUP0C and PSB (Program Specification Block) PSBPAUTB for the IMS execution.

**Business Context**: This JCL job is likely part of a larger system that manages authorizations and periodically removes expired ones to maintain data integrity and security.

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | Executes the IMS BMP region to run the authorization deletion program. |

## Paragraphs/Procedures

### STEP01
This step executes the IMS program DFSRRC00 in a Batch Message Processing (BMP) region. It specifies the program to be executed (CBPAUP0C) and the PSB (PSBPAUTB) to be used. The STEPLIB DD statements define the libraries containing the IMS RESLIB and the application load library. DFSRESLB points to the IMS RESLIB. PROCLIB defines the IMS procedure library. DFSSEL points to the IMS SDFSRESL dataset. The IMS DD statement defines the PSBLIB and DBDLIB datasets. SYSIN provides input parameters (00,00001,00001,Y). SYSOUX, SYSOUT, SYSABOUT, ABENDAID, SYSPRINT, SYSUDUMP, and IMSERR are DD statements for SYSOUT. IEFRDER and IMSLOGR are DUMMY datasets. The PARM parameter passes 'BMP,CBPAUP0C,PSBPAUTB' to DFSRRC00, indicating a BMP region, the application program name, and the PSB name, respectively. This step is the core execution unit of the JCL, responsible for invoking the IMS program to perform the authorization deletion.

## Open Questions

- ? What specific criteria are used to determine which authorizations are 'expired'?
  - Context: The JCL deletes expired authorizations, but the logic for determining expiration is within the CBPAUP0C program, which is not available.
