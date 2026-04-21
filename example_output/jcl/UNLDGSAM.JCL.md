# UNLDGSAM

**File**: `jcl/UNLDGSAM.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-04-21 13:48:23.229358

## Purpose

This JCL job executes the IMS program DFSRRC00 to unload GSAM databases. It specifies the IMS region, libraries, and input/output datasets required for the unload process.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.PAUTDB.ROOT.GSAM | IOType.FILE_SEQUENTIAL | Input GSAM database file to be unloaded. This file contains the root segment data. |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.GSAM | IOType.FILE_SEQUENTIAL | Input GSAM database file to be unloaded. This file contains the child segment data. |
| OEMA.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSBLIB library containing PSB definitions. |
| OEMA.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBDLIB library containing DBD definitions. |
| OEM.IMS.IMSP.PAUTHDB | IOType.FILE_SEQUENTIAL | Input dataset for IMS PAUTHDB. |
| OEM.IMS.IMSP.PAUTHDBX | IOType.FILE_SEQUENTIAL | Input dataset for IMS PAUTHDBX. |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | IMS PROCLIB member containing DFSVSMDB parameters. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | System print output for job execution messages. |
| SYSUDUMP | IOType.REPORT | System dump output for debugging purposes. |
| IMSERR | IOType.REPORT | IMS error output. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | Executes the IMS DL/I region controller to unload the GSAM database. |

## Paragraphs/Procedures

### UNLDGSAM
This JCL job unloads a GSAM database using the IMS program DFSRRC00. The job begins by defining the job characteristics, including job name, class, message class, region size, notification settings, and time limit. It then executes the DFSRRC00 program with specific parameters to initiate the database unload process. The STEPLIB DD statements define the libraries required for the IMS execution, including SDFSRESL and the application load library. The IMS DD statement specifies the PSBLIB and DBDLIB datasets, which contain the program and database definitions. The PASFILOP and PADFILOP DD statements define the input GSAM database files for the root and child segments, respectively. Finally, the SYSPRINT, SYSUDUMP, and IMSERR DD statements define the output datasets for job execution messages, system dumps, and IMS error messages.
