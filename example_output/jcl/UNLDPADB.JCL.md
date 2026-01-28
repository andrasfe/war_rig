# UNLDPADB

**File**: `jcl/UNLDPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-28 14:55:41.593614

## Purpose

This JCL unloads the PAUTDB IMS database to sequential files. It executes the IMS program DFSRRC00 with the PARM parameter specifying the DLI function and the PAUDBUNL application program. It also deletes and recreates the output files.

**Business Context**: UNKNOWN

## Inputs

| Name | Type | Description |
|------|------|-------------|
| OEM.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB |
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSBLIB |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBDLIB |
| OEM.IMS.IMSP.PAUTHDB | IOType.FILE_SEQUENTIAL | PAUTHDB IMS database |
| OEM.IMS.IMSP.PAUTHDBX | IOType.FILE_SEQUENTIAL | PAUTHDBX IMS database |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | DFSVSMDB proc |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO | IOType.FILE_SEQUENTIAL | Unloaded PAUTDB root segment data |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO | IOType.FILE_SEQUENTIAL | Unloaded PAUTDB child segment data |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Dummy program to delete datasets |
| DFSRRC00 | CallType.STATIC_CALL | IMS program to unload the database |

## Paragraphs/Procedures

### STEP0
This step executes the IEFBR14 program, a dummy program that performs no processing. It is used here to delete the existing output datasets AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO and AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO before they are recreated in a later step. The DD statements DD1 and DD2 define these datasets and specify DISP=(OLD,DELETE,DELETE), which ensures that the datasets are deleted if they exist. This step ensures that the subsequent step starts with a clean slate, preventing potential issues with pre-existing data in the output files. No input data is read, and no business logic is applied in this step. The SYSPRINT, SYSOUT, and SYSDUMP DD statements define the output datasets for system messages and dumps.

### STEP01
This step executes the IMS program DFSRRC00 to unload the PAUTDB database. The PARM parameter specifies 'DLI,PAUDBUNL,PAUTBUNL,,,,,,,,,,,N', indicating a DLI call to the PAUDBUNL application program, which is responsible for unloading the database. The STEPLIB DD statement defines the libraries containing the necessary IMS modules, including OEMA.IMS.IMSP.SDFSRESL, OEMA.IMS.IMSP.SDFSRESL.V151, and AWS.M2.CARDDEMO.LOADLIB. The DFSRESLB DD statement specifies the IMS RESLIB. The IMS DD statement defines the PSBLIB and DBDLIB. The OUTFIL1 and OUTFIL2 DD statements define the output datasets AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO and AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO, respectively, specifying their attributes such as LRECL, BLKSIZE, RECFM, UNIT, and SPACE. The DDPAUTP0 and DDPAUTX0 DD statements define the PAUTHDB and PAUTHDBX datasets. The DFSVSAMP DD statement defines the DFSVSMDB proc. The remaining DD statements (IMSLOGR, IEFRDER, SYSPRINT, SYSUDUMP, IMSERR) define various output datasets for logging and error reporting.

## Open Questions

- ? What is the purpose of the PAUDBUNL application program?
  - Context: The JCL calls DFSRRC00 with PAUDBUNL, but the function of this program is not clear from the JCL itself.
