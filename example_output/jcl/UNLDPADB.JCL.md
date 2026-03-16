# UNLDPADB

**File**: `jcl/UNLDPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-03-16 20:02:52.710974

## Purpose

This JCL job unloads the PAUTDB database using the IMS program DFSRRC00. It first deletes the existing root and child files, then executes the unload utility to create new sequential files containing the database data. The job also defines the necessary IMS libraries and datasets for the unload process.

**Business Context**: Database maintenance and data extraction for the PAUTDB database.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSB library. |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBD library. |
| OEM.IMS.IMSP.PAUTHDB | IOType.FILE_SEQUENTIAL | PAUTDB database. |
| OEM.IMS.IMSP.PAUTHDBX | IOType.FILE_SEQUENTIAL | PAUTDB index database. |
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB library. |
| OEMA.IMS.IMSP.SDFSRESL.V151 | IOType.FILE_SEQUENTIAL | IMS RESLIB library version 151. |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Load library containing the required modules. |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | DFSVSMDB member in the PROCLIB. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO | IOType.FILE_SEQUENTIAL | Unloaded root segment data of PAUTDB database. |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO | IOType.FILE_SEQUENTIAL | Unloaded child segment data of PAUTDB database. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Dummy program used to delete existing datasets. |
| DFSRRC00 | CallType.STATIC_CALL | IMS program to unload the PAUTDB database. |

## Paragraphs/Procedures

### STEP0
This step executes the IEFBR14 program, a dummy program, to delete the existing root and child datasets before the unload process. The DD1 DD statement defines the root dataset AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO, and the DD2 DD statement defines the child dataset AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO. The DISP parameter (OLD,DELETE,DELETE) ensures that the datasets are deleted if they exist. This step prepares the environment for the subsequent unload step by removing any pre-existing output files, ensuring a clean unload process. The SYSPRINT, SYSOUT, and SYSDUMP DD statements define the output datasets for the job.

### STEP01
This step executes the IMS program DFSRRC00 to unload the PAUTDB database. The PARM parameter specifies the execution parameters for DFSRRC00, including 'DLI' to indicate a DLI batch job, 'PAUDBUNL' as the PSB name, and 'PAUTBUNL' as the program name. The STEPLIB DD statements define the libraries containing the necessary IMS modules, including OEMA.IMS.IMSP.SDFSRESL, OEMA.IMS.IMSP.SDFSRESL.V151, and AWS.M2.CARDDEMO.LOADLIB. The DFSRESLB DD statement defines the IMS RESLIB library. The IMS DD statement defines the PSBLIB and DBDLIB libraries. The OUTFIL1 and OUTFIL2 DD statements define the output datasets for the unloaded root and child segments, respectively, specifying their DCB attributes and space allocation. The DDPAUTP0 and DDPAUTX0 DD statements define the PAUTDB and its index database. The remaining DD statements (DFSVSAMP, IMSLOGR, IEFRDER, SYSPRINT, SYSUDUMP, IMSERR) define various system datasets and output datasets for the IMS execution.
