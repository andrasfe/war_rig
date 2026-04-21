# DBPAUTP0

**File**: `jcl/DBPAUTP0.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-04-21 13:48:17.037653

## Purpose

This JCL job unloads the DBD DBPAUTP0 to a sequential dataset. It first deletes the output dataset if it exists, then executes the IMS program DFSRRC00 to perform the unload.

**Business Context**: UNKNOWN

## Inputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 | IOType.FILE_SEQUENTIAL | Output dataset that is deleted if it exists. |
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Load library. |
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSBLIB |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBDLIB |
| OEM.IMS.IMSP.PAUTHDB | IOType.FILE_SEQUENTIAL | PAUTHDB dataset |
| OEM.IMS.IMSP.PAUTHDBX | IOType.FILE_SEQUENTIAL | PAUTHDBX dataset |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | DFSVSMDB proc |
| OEM.IMS.IMSP.RECON1 | IOType.FILE_SEQUENTIAL | RECON1 dataset |
| OEM.IMS.IMSP.RECON2 | IOType.FILE_SEQUENTIAL | RECON2 dataset |
| OEM.IMS.IMSP.RECON3 | IOType.FILE_SEQUENTIAL | RECON3 dataset |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 | IOType.FILE_SEQUENTIAL | Unloaded DBD data. |
| SYSPRINT | IOType.REPORT | Job log and program output. |
| SYSUDUMP | IOType.REPORT | Memory dump in case of abend. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Deletes the output dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 if it exists. |
| DFSRRC00 | CallType.STATIC_CALL | Unloads the DBD DBPAUTP0. |

## Paragraphs/Procedures

### STEPDEL
This step deletes the output dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 before the unload process. It executes the utility program IEFBR14, which is used to delete and/or allocate datasets. The SYSPRINT DD statement defines the output for system messages. The SYSUT1 DD statement specifies the dataset to be deleted, with DISP=(MOD,DELETE) indicating that the dataset should be deleted if it exists. The UNIT and SPACE parameters define the storage unit and space allocation for the dataset. This step ensures that a clean output dataset is available for the subsequent unload process.

### UNLOAD
This step executes the IMS program DFSRRC00 to unload the DBD DBPAUTP0. The PARM parameter specifies the control region parameters, including ULU (Unload Utility), DFSURGU0 (Unload control statement), and DBPAUTP0 (Database name). The STEPLIB DD statements define the libraries containing the IMS modules. The DFSRESLB DD statement specifies the IMS RESLIB. The IMS DD statements define the PSBLIB and DBDLIB datasets. The DFSURGU1 DD statement defines the output dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 where the unloaded DBD data will be written. The DDPAUTP0 and DDPAUTX0 DD statements define the PAUTHDB and PAUTHDBX datasets. The DFSVSAMP DD statement defines the DFSVSMDB proc. The DFSCTL DD statement contains the SBPARM ACTIV=COND control statement. The SYSUDUMP DD statement defines the output dataset for memory dumps. The RECON* DD statements define the RECON datasets. DFSWRK01 and DFSSRT01 are dummy datasets.
