# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-02-03 21:08:13.234045

## Purpose

This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and necessary datasets for the IMS environment.

**Business Context**: Database maintenance and loading.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB dataset. |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Application load library. |
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSB library. |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBD library. |
| AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO | IOType.FILE_SEQUENTIAL | Input file for the PAUTDB root segment. |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO | IOType.FILE_SEQUENTIAL | Input file for the PAUTDB child segment. |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | IMS VSAM definition. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | System print output. |
| SYSUDUMP | IOType.REPORT | System dump output. |
| IMSERR | IOType.REPORT | IMS error output. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS program execution. |

## Paragraphs/Procedures

### LOADPADB JOB
This JCL JOB statement initiates the LOADPADB job, specifying job characteristics such as job name ('M2APP'), class ('A'), message class ('H'), message level ((1,1)), region size ('0M'), notification ID ('&SYSUID'), and time limit ('1440'). The job's primary purpose is to execute the IMS program DFSRRC00 to load the PAUTDB database. It does not directly consume any input data but defines the environment for the subsequent steps. The job produces system messages and potential error outputs based on the execution of the IMS program. No specific business logic is implemented at the JCL level; the logic resides within the called IMS program. Error handling is limited to the JCL's ability to capture system dumps and error messages. The job calls the STEP01 execution step, which in turn calls the IMS program DFSRRC00.

### STEP01 EXEC
This EXEC statement defines the step STEP01, which executes the IMS program DFSRRC00. The PARM parameter specifies 'BMP,PAUDBLOD,PSBPAUTB', indicating a Batch Message Processing region, the program to be loaded (PAUDBLOD), and the PSB (Program Specification Block) to be used (PSBPAUTB). This step consumes the IMS RESLIB, PSBLIB, and DBDLIB datasets, along with the input files for the PAUTDB root and child segments. It produces SYSPRINT, SYSUDUMP, and IMSERR outputs for logging and debugging. The business logic is encapsulated within the DFSRRC00 program, which performs the actual database loading. Error handling is managed by IMS and the operating system, with potential dumps and error messages directed to the specified SYSOUT datasets. This step calls the DFSRRC00 program.
