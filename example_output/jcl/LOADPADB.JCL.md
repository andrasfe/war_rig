# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-28 14:55:30.595680

## Purpose

This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and input files required for the database load process.

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
| DFSRRC00 | CallType.STATIC_CALL | Execute the IMS BMP region to load the PAUTDB database. |

## Paragraphs/Procedures

### STEP01
This step executes the IMS program DFSRRC00 in a BMP region to load the PAUTDB database. It defines the necessary libraries (STEPLIB, DFSRESLB, IMS) and input files (INFILE1, INFILE2) required for the database load process. The PARM parameter specifies the execution environment as BMP, the transaction PAUDBLOD, and the PSB PSBPAUTB. The STEPLIB DD statements define the libraries containing the IMS modules and the application load library. The DFSRESLB DD statement defines the IMS RESLIB library. The IMS DD statements define the PSB and DBD libraries. INFILE1 and INFILE2 DD statements define the input files for the root and child segments of the PAUTDB database, respectively. DFSVSAMP defines the VSAM parameters. The SYSPRINT, SYSUDUMP, and IMSERR DD statements define the output datasets for system messages, dumps, and IMS error messages, respectively.

## Open Questions

- ? What is the exact format and content of the PAUTDB database input files?
  - Context: The JCL defines the input files but does not specify the record layouts or data types.
