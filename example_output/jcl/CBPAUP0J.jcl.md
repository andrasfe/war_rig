# CBPAUP0J

**File**: `jcl/CBPAUP0J.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 17:36:40.120235

## Purpose

This JCL job CBPAUP0J executes the IMS Batch Message Processing (BMP) program CBPAUP0C using DFSRRC00 to delete expired authorizations from an IMS database. It references necessary IMS libraries and provides inline SYSIN data for processing parameters. Output is directed to multiple SYSOUT datasets for logs and dumps.

**Business Context**: Purges expired authorizations in a card demo system (CARDDEMO class) using IMS resources under PSBPAUTB.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| SYSIN | IOType.PARAMETER | Inline IMS transaction input data: 00,00001,00001,Y specifying processing details like PCB position or message format |
| IMS.PSBLIB | IOType.FILE_SEQUENTIAL | Program Specification Block library containing PSBPAUTB definition |
| IMS.DBDLIB | IOType.FILE_SEQUENTIAL | Database Definition library for IMS database access |
| STEPLIB | IOType.OTHER | Load libraries: IMS.SDFSRESL and XXXXXXXX.PROD.LOADLIB containing DFSRRC00 and CBPAUP0C |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard print output from IMS execution |
| SYSUDUMP | IOType.REPORT | System dump output on abend |
| SYSOUT | IOType.REPORT | General job output |
| ABENDAID | IOType.REPORT | Abend aid diagnostics |
| IMSERR | IOType.REPORT | IMS error messages |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS region controller to execute BMP region |
| CBPAUP0C | CallType.DYNAMIC_CALL | Application program to delete expired authorizations using PSBPAUTB |

## Paragraphs/Procedures

### STEP01
This is the only execution step in the JCL job, named STEP01, serving as the primary orchestration point to run the IMS BMP program for deleting expired authorizations. It consumes inputs from multiple DD statements including STEPLIB for load modules (lines 26-27), DFSRESLB and PROCLIB for IMS resources (28-29), DFSSEL (31), IMS PSBLIB and DBDLIB for control blocks (33-34), and SYSIN inline data (37) which provides IMS-specific parameters like message sequence or PCB details. It produces outputs by allocating several SYSOUT datasets such as SYSOUX (39), SYSOUT (40), SYSABOUT (41), ABENDAID (42), SYSPRINT (45), SYSUDUMP (46), and IMSERR (47) to capture prints, logs, dumps, and errors. There are no business logic decisions implemented directly in the JCL; all processing logic is delegated to the executed programs DFSRRC00 and CBPAUP0C. Error handling is implicit through allocated dump and error DD statements like SYSUDUMP for system dumps and IMSERR for IMS-specific errors, with dummy DDs IEFRDER (43) and IMSLOGR (44) to suppress unnecessary outputs. The step calls DFSRRC00 statically via EXEC PGM= and dynamically invokes CBPAUP0C via PARM to perform the authorization purge under PSBPAUTB. Job parameters like REGION=0M and NOTIFY=&SYSUID (lines 1-2) ensure resource allocation and notification. Overall, STEP01 sets up the complete IMS execution environment without any loops or conditional logic.

## Open Questions

- ? Precise format and meaning of SYSIN data '00,00001,00001,Y'
  - Context: IMS-specific parameters (e.g., PCB number, message number, flags) not self-documenting in JCL
- ? Specific IMS database/segment accessed for deletions
  - Context: JCL references libraries but not data names; handled in CBPAUP0C
