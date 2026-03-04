# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-03-04 04:45:13.845013

## Purpose

This JCL job executes an IMS program DFSRRC00 to load the PAUTDB database using the BMP (Batch Message Processing) region. It specifies the program, PSB, and necessary datasets for the IMS environment.

**Business Context**: This job is likely part of a larger process for database maintenance or initial data loading within an IMS environment.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB library. |
| OEMA.IMS.IMSP.SDFSRESL.V151 | IOType.FILE_SEQUENTIAL | IMS RESLIB library version 151. |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Application load library. |
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSB library. |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBD library. |
| AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO | IOType.FILE_SEQUENTIAL | Input file for the PAUTDB root segment. |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO | IOType.FILE_SEQUENTIAL | Input file for the PAUTDB child segment. |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | IMS VSAM definition. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | System print output for job execution messages. |
| SYSUDUMP | IOType.REPORT | System dump output for debugging purposes. |
| IMSERR | IOType.REPORT | IMS error output. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | Executes the IMS program specified in the PARM parameter. |

## Paragraphs/Procedures

### LOADPADB
This JCL defines a job named LOADPADB to execute an IMS batch program. The job specifies job characteristics such as class, message class, region size, user ID for notification, and time limit. It then executes the IMS program DFSRRC00, which is a control region program, in BMP mode to load the PAUTDB database. The PARM parameter passes the BMP region type, the program name PAUDBLOD, and the PSB name PSBPAUTB to the IMS control region. The JCL defines the necessary datasets for the IMS environment, including RESLIB, PSBLIB, DBDLIB, and the input files for the PAUTDB database. It also defines dummy datasets for IMS logging and error reporting datasets for SYSPRINT, SYSUDUMP and IMSERR.
