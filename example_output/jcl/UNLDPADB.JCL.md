# UNLDPADB

**File**: `jcl/UNLDPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-04-21 13:48:04.511311

## Purpose

This JCL job unloads the PAUTDB database using the IMS program DFSRRC00. It first deletes existing database files and then recreates them after the unload.

**Business Context**: Database maintenance and migration.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| OEM.IMS.IMSP.PAUTHDB | IOType.FILE_VSAM | Input database to be unloaded. |
| OEM.IMS.IMSP.PAUTHDBX | IOType.FILE_VSAM | Index file for the database to be unloaded. |
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB library. |
| OEMA.IMS.IMSP.SDFSRESL.V151 | IOType.FILE_SEQUENTIAL | IMS RESLIB library version 151. |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Load library containing the IMS program. |
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSB library. |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBD library. |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | IMS VSAM definition. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO | IOType.FILE_VSAM | Unloaded root segment data. |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO | IOType.FILE_VSAM | Unloaded child segment data. |
| SYSOUT | IOType.REPORT | System output for job execution. |
| SYSPRINT | IOType.REPORT | System print output. |
| SYSDUMP | IOType.REPORT | System dump output. |
| SYSUDUMP | IOType.REPORT | System user dump output. |
| IMSERR | IOType.REPORT | IMS error output. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS program to unload the database. |
| IEFBR14 | CallType.STATIC_CALL | Dummy program to delete the existing database files. |

## Paragraphs/Procedures

### STEP0
This step executes the dummy program IEFBR14 to delete the existing database files AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO and AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO. The DD statements DD1 and DD2 define these files and specify DISP=(OLD,DELETE,DELETE), which ensures that the files are deleted if they exist. This step prepares the environment for the subsequent step that unloads the database and recreates these files. No specific input data is read or processed in this step. The output is the deletion of the specified datasets. The program IEFBR14 does not perform any actual processing; it simply returns a zero return code.

### STEP01
This step executes the IMS program DFSRRC00 to unload the PAUTDB database. The PARM parameter specifies the execution parameters for DFSRRC00, including 'DLI' to indicate a DLI batch job, 'PAUDBUNL' and 'PAUTBUNL' as the PSB and program names respectively, and 'N' for some other option. The STEPLIB DD statements define the libraries required to execute DFSRRC00, including the IMS RESLIB and the application load library. The DFSRESLB DD statement specifies the IMS RESLIB. The IMS DD statement defines the PSB and DBD libraries. The OUTFIL1 and OUTFIL2 DD statements define the output files where the unloaded data will be written. The DDPAUTP0 and DDPAUTX0 DD statements define the input database and its index. The DFSVSAMP DD statement defines the VSAM parameters. The IMSLOGR and IEFRDER DD statements are dummy datasets. The SYSPRINT, SYSUDUMP, and IMSERR DD statements define the output datasets for system messages, user dumps, and IMS errors, respectively.
