# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-02-10 17:21:07.633684

## Purpose

This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and necessary datasets for the IMS environment and input files.

**Business Context**: Database maintenance and loading.

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
| SYSPRINT | IOType.REPORT | System print output. |
| SYSUDUMP | IOType.REPORT | System dump output. |
| IMSERR | IOType.REPORT | IMS error output. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS program execution. |

## Paragraphs/Procedures

### LOADPADB
This JCL defines a job named LOADPADB to load the PAUTDB database using an IMS BMP region. The job specifies job characteristics such as class, message class, region size, notification, and time limit. It then executes the IMS program DFSRRC00 with parameters indicating a BMP region, the PAUDBLOD program, and the PSBPAUTB PSB. The STEPLIB DD statements define the libraries required for the IMS execution, including the IMS RESLIB and the application load library. The IMS DD statement specifies the PSB and DBD libraries. INFILE1 and INFILE2 define the input datasets for the PAUTDB root and child segments, respectively. DFSVSAMP defines the VSAM buffer pool parameters. Finally, SYSPRINT, SYSUDUMP, and IMSERR define the system print, system dump, and IMS error output datasets, respectively. The job's primary purpose is to load the PAUTDB database using the specified IMS configuration and input data. It consumes the input datasets defined by INFILE1 and INFILE2 and produces output reports defined by SYSPRINT, SYSUDUMP, and IMSERR. No specific business logic is implemented within the JCL itself, as it primarily defines the execution environment for the IMS program.
