# CBPAUP0J

**File**: `jcl/CBPAUP0J.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 14:20:01.500052

## Purpose

This JCL defines a batch job that executes the IMS batch message processing (BMP) region controller DFSRRC00 to run the application program CBPAUP0C using PSB PSBPAUTB. The job's purpose is to delete expired authorizations from an IMS database. It specifies required IMS libraries, control datasets, input parameters via SYSIN, and output destinations for logs and dumps.

**Business Context**: Supports a card demo ('CARDDEMO') business process by purging expired authorizations from IMS to maintain data integrity.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| SYSIN | IOType.PARAMETER | Control input data '00,00001,00001,Y' passed to DFSRRC00 for IMS region configuration |
| IMS (PSBLIB/DBDLIB) | IOType.IMS_SEGMENT | IMS PSB and DBD libraries defining access for PSBPAUTB to authorization data |
| STEPLIB | IOType.OTHER | Load libraries containing DFSRRC00, CBPAUP0C, and IMS modules |
| DFSRESLB | IOType.OTHER | IMS resource library for region execution |
| PROCLIB | IOType.OTHER | IMS procedure library |
| DFSSEL | IOType.OTHER | IMS selection list dataset |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| IMS DATABASE (PSBPAUTB) | IOType.IMS_SEGMENT | IMS database updated by deleting expired authorization records |
| SYSOUX | IOType.REPORT | Job step output dataset X |
| SYSOUT | IOType.REPORT | Standard job output |
| SYSABOUT | IOType.REPORT | Informational output |
| SYSPRINT | IOType.REPORT | Print file for job output |
| SYSUDUMP | IOType.REPORT | System dump for diagnostics |
| IMSERR | IOType.REPORT | IMS error messages |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS region controller to execute BMP application |
| CBPAUP0C | CallType.DYNAMIC_CALL | Application logic to delete expired authorizations using PSBPAUTB |

## Business Rules

- **BR001**: Execute deletion of expired authorizations in IMS database

## Paragraphs/Procedures

### STEP01
This is the only step in the JCL job, serving as the primary execution point for the IMS batch processing. Its main role is to invoke the IMS region controller DFSRRC00 with PARM='BMP,CBPAUP0C,PSBPAUTB' to load and run the CBPAUP0C application under PSBPAUTB for deleting expired authorizations (lines 24-25). It consumes inputs from SYSIN containing '00,00001,00001,Y' for region parameters (line 37), and multiple DD datasets providing IMS libraries such as STEPLIB (lines 26-27), DFSRESLB (28), PROCLIB (29), DFSSEL (31), and IMS PSB/DBD libraries (33-34). Outputs are produced to various SYSOUT=* datasets including SYSOUX, SYSOUT, SYSABOUT, ABENDAID, SYSPRINT, SYSUDUMP, and IMSERR for logging, printing, and error reporting (lines 39-47). Dummy DDs for IEFRDER and IMSLOGR suppress unnecessary reader and log outputs (lines 43-44). No business decisions are made in the JCL itself; all logic resides in CBPAUP0C. Error handling is passive, relying on standard IMS abend processing with dumps directed to specified SYSOUTs. The step has no subordinate paragraphs but effectively calls DFSRRC00 which dynamically loads CBPAUP0C. Upon completion, the job ends as there are no further steps.

## Open Questions

- ? What is the exact format and meaning of SYSIN data '00,00001,00001,Y'?
  - Context: Not explained in JCL comments; standard for DFSRRC00 but specifics unclear
- ? What specific IMS databases and segments are accessed by PSB PAUTB?
  - Context: PSBLIB provided but no PCB or segment details in JCL
- ? Detailed logic of CBPAUP0C for identifying and deleting expired authorizations?
  - Context: JCL only submits the program; no code provided
