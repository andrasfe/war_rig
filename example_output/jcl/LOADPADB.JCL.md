# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-02-27 14:44:58.528304

## Purpose

This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program DFSRRC00, the BMP region, the database name PAUDBLOD, and the PSB name PSBPAUTB as parameters.

**Business Context**: Database maintenance and loading.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB library. |
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
| DFSRRC00 | CallType.STATIC_CALL | Executes the IMS BMP region to load the PAUTDB database. |

## Paragraphs/Procedures

### LOADPADB
This JCL defines a job named LOADPADB to execute an IMS batch message processing (BMP) program for loading the PAUTDB database. The job specifies job characteristics such as class, message class, region size, notification, and time limit. It then executes the IMS program DFSRRC00 with parameters indicating a BMP region, the database name PAUDBLOD, and the PSB name PSBPAUTB. The JCL defines the necessary DD statements for the IMS RESLIB, application load library, PSB library, DBD library, input files for the root and child segments of the PAUTDB database, and the VSAM definition. It also defines dummy DD statements for IMSLOGR and IEFRDER, and SYSOUT DD statements for SYSPRINT, SYSUDUMP, and IMSERR to capture program output and error messages. The purpose of this JCL is to load the PAUTDB database using a BMP region, utilizing the specified input files and IMS libraries.
