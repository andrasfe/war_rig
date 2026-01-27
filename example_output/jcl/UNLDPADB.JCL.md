# UNLDPADB

**File**: `jcl/UNLDPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-27 23:06:17.021503

## Purpose

This JCL job unloads the PAUTDB database using the IMS program DFSRRC00. It deletes existing root and child files, creates new ones, and then executes the unload utility.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO | IOType.FILE_VSAM | Existing root file to be unloaded and deleted. It is then recreated as an output file. |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO | IOType.FILE_VSAM | Existing child file to be unloaded and deleted. It is then recreated as an output file. |
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB |
| OEMA.IMS.IMSP.SDFSRESL.V151 | IOType.FILE_SEQUENTIAL | IMS RESLIB version 151 |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Load library for the application |
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSBLIB |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBDLIB |
| OEM.IMS.IMSP.PAUTHDB | IOType.FILE_SEQUENTIAL | PAUTHDB dataset |
| OEM.IMS.IMSP.PAUTHDBX | IOType.FILE_SEQUENTIAL | PAUTHDBX dataset |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | DFSVSMDB proc from IMS PROCLIB |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO | IOType.FILE_VSAM | Recreated root file after unloading |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO | IOType.FILE_VSAM | Recreated child file after unloading |
| SYSPRINT | IOType.REPORT | System print output |
| SYSOUT | IOType.REPORT | System output |
| SYSDUMP | IOType.REPORT | System dump output |
| SYSUDUMP | IOType.REPORT | System user dump output |
| IMSERR | IOType.REPORT | IMS error output |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Dummy program used to delete and define the output files |
| DFSRRC00 | CallType.STATIC_CALL | IMS program to unload the PAUTDB database |

## Paragraphs/Procedures

### STEP0
This step executes the IEFBR14 program, a dummy program, to pre-allocate and delete the existing root and child files (AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO and AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO). The DD1 and DD2 DD statements define these files with a disposition of OLD, DELETE, DELETE, ensuring they are deleted before the next step. This step prepares the environment for the subsequent unloading process by removing the old database files. No specific business logic is implemented in this step, as it primarily focuses on file management. It does not call any other programs or paragraphs.

### STEP01
This step executes the IMS program DFSRRC00 to unload the PAUTDB database. The PARM parameter specifies 'DLI,PAUDBUNL,PAUTBUNL,,,,,,,,,,,N', indicating a DLI call to the PAUDBUNL program using the PAUTBUNL PSB. The STEPLIB DD statements define the libraries required to execute the IMS program, including OEMA.IMS.IMSP.SDFSRESL, OEMA.IMS.IMSP.SDFSRESL.V151, and AWS.M2.CARDDEMO.LOADLIB. The DFSRESLB DD statement points to the IMS RESLIB. The IMS DD statement defines the PSBLIB and DBDLIB. The OUTFIL1 and OUTFIL2 DD statements define the output files for the unloaded root and child segments, respectively, specifying their DCB attributes and space allocation. The DDPAUTP0 and DDPAUTX0 DD statements define the PAUTHDB and PAUTHDBX datasets. The DFSVSAMP DD statement defines the DFSVSMDB proc from OEMPP.IMS.V15R01MB.PROCLIB. The remaining DD statements define dummy datasets for IMS logging and error handling, as well as SYSOUT datasets for various outputs. This step is crucial for extracting the data from the IMS database into sequential files. It does not call any other programs or paragraphs directly, but relies on the DFSRRC00 program to perform the unload operation.
