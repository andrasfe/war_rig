# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-02-24 17:39:24.239379

## Purpose

This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and necessary datasets for the IMS environment.

**Business Context**: Database maintenance and loading for the CARDDEMO application.

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
| DFSRRC00 | CallType.STATIC_CALL | IMS control region program to load the PAUTDB database. |

## Paragraphs/Procedures

### LOADPADB
This JCL defines a job named LOADPADB to execute an IMS database load process. The job is assigned attributes such as job class 'A', message class 'H', region size '0M', and a time limit of 1440 minutes. It begins by specifying the JCLLIB to be used (VIPINGP.CNTL.PROCLIB). The core of the job is STEP01, which executes the IMS control region program DFSRRC00 with parameters indicating a BMP region, the database load transaction PAUDBLOD, and the PSB PSBPAUTB. The STEPLIB DD statements define the load libraries required for the IMS execution, including the IMS RESLIB and the application load library. The IMS DD statement specifies the PSBLIB and DBDLIB datasets. INFILE1 and INFILE2 define the input datasets for the root and child segments of the PAUTDB database, respectively. DFSVSAMP defines the VSAM parameters. Finally, the SYSPRINT, SYSUDUMP, and IMSERR DD statements define the output datasets for system messages, dumps, and IMS error messages. This JCL orchestrates the execution of the IMS load utility, providing the necessary environment and input data for the database population process.

## Open Questions

- ? What is the exact structure of the PAUTDB database being loaded?
  - Context: The JCL specifies the input files for the root and child segments, but the specific data structure is not defined here.
