# CBPAUP0J

**File**: `jcl/CBPAUP0J.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-04-21 13:48:44.834989

## Purpose

This JCL executes an IMS program (DFSRRC00) to delete expired authorizations. It defines the execution environment for the IMS program, including specifying the program to run (CBPAUP0C) and the PSB (PSBPAUTB), along with necessary libraries and input parameters.

**Business Context**: This job is likely part of a larger authorization management process, ensuring that expired authorizations are removed from the system, maintaining security and compliance.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| SYSIN | IOType.PARAMETER | Input parameters for the IMS program, including configuration settings and potentially data selection criteria. The example shows '00,00001,00001,Y' as input. |
| IMS.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS system library containing required modules and routines. |
| XXXXXXXX.PROD.LOADLIB | IOType.FILE_SEQUENTIAL | Production load library containing application-specific modules. |
| IMS.PROCLIB | IOType.FILE_SEQUENTIAL | IMS procedure library. |
| IMS.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSB library containing Program Specification Blocks. |
| IMS.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBD library containing Database Description blocks. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSOUX | IOType.REPORT | System output for user messages. |
| SYSOUT | IOType.REPORT | Standard system output. |
| SYSABOUT | IOType.REPORT | System output for ABEND information. |
| ABENDAID | IOType.REPORT | System output for ABEND aid information. |
| SYSPRINT | IOType.REPORT | System print output. |
| SYSUDUMP | IOType.REPORT | System user dump output. |
| IMSERR | IOType.REPORT | IMS error output. |
| IEFRDER | IOType.OTHER | Dummy dataset for IEFRDER. |
| IMSLOGR | IOType.OTHER | Dummy dataset for IMSLOGR. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | This is the IMS control region program that executes the specified BMP (Batch Message Processing) application. |

## Paragraphs/Procedures

### STEP01
This step executes the IMS program DFSRRC00. It defines the program to be executed and its parameters. The PARM parameter specifies 'BMP' indicating a Batch Message Processing region, 'CBPAUP0C' which is the application program to be executed, and 'PSBPAUTB' which is the Program Specification Block (PSB) to be used. The STEPLIB DD statements define the libraries containing the necessary modules for the IMS execution. DFSRESLB points to the IMS SDFSRESL library. SYSIN provides input parameters to the CBPAUP0C program. The remaining DD statements define the various output datasets for system messages, ABEND information, and other diagnostic data. IEFRDER and IMSLOGR are defined as DUMMY datasets, indicating that no actual data is written to these datasets. The overall purpose of this step is to initiate and control the execution of the CBPAUP0C program within the IMS environment, providing it with the necessary libraries, parameters, and input/output definitions.
