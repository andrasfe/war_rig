# CBPAUP0J

**File**: `jcl/CBPAUP0J.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-27 23:06:14.375641

## Purpose

This JCL executes an IMS program (CBPAUP0C) to delete expired authorizations. It defines the execution environment for the IMS program, including necessary libraries and input parameters.

**Business Context**: UNKNOWN

## Inputs

| Name | Type | Description |
|------|------|-------------|
| SYSIN | IOType.PARAMETER | Control input for the IMS program, likely containing parameters related to authorization expiration criteria. The example shows '00,00001,00001,Y' as input. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSOUX | IOType.REPORT | System output. |
| SYSOUT | IOType.REPORT | Standard system output. |
| SYSABOUT | IOType.REPORT | System output for ABEND information. |
| ABENDAID | IOType.REPORT | System output for ABEND debugging. |
| SYSPRINT | IOType.REPORT | System print output. |
| SYSUDUMP | IOType.REPORT | System dump output. |
| IMSERR | IOType.REPORT | IMS error output. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | Executes the IMS control region to run the specified BMP program. |

## Paragraphs/Procedures

### STEP01
This step executes the IMS program CBPAUP0C within the IMS control region. It uses the DFSRRC00 program to initiate the IMS environment and specifies 'BMP' as the execution type, indicating a Batch Message Processing program. The PSBPAUTB parameter likely identifies the Program Specification Block (PSB) to be used for this execution, defining the program's access to IMS databases. The STEPLIB DD statements define the load libraries required for the execution, including the IMS.SDFSRESL and XXXXXXXX.PROD.LOADLIB. The DFSRESLB DD statement specifies the IMS resident library. The SYSIN DD statement provides input parameters to the CBPAUP0C program, potentially controlling the criteria for deleting expired authorizations. Several SYSOUT DD statements define output datasets for various system and program messages, including standard output, ABEND information, and IMS error messages. IEFRDER and IMSLOGR are DUMMY datasets, likely placeholders for log data that is not being captured in this execution.

## Open Questions

- ? What is the exact purpose of the input parameters provided via SYSIN?
  - Context: The meaning of '00,00001,00001,Y' is unclear without further documentation or the source code of CBPAUP0C.
- ? What specific business process does deleting expired authorizations support?
  - Context: The JCL does not provide enough information to determine the business context.
