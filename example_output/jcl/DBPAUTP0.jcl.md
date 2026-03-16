# DBPAUTP0

**File**: `jcl/DBPAUTP0.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-03-16 20:03:06.056224

## Purpose

This JCL job unloads the DBD DBPAUTP0 to a sequential dataset. It first deletes the output dataset if it exists, then executes the IMS program DFSRRC00 to perform the unload.

**Business Context**: UNKNOWN

## Inputs

| Name | Type | Description |
|------|------|-------------|
| OEM.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB library. |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Load library. |
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSBLIB library. |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBDLIB library. |
| OEM.IMS.IMSP.PAUTHDB | IOType.FILE_SEQUENTIAL | PAUTHDB dataset. |
| OEM.IMS.IMSP.PAUTHDBX | IOType.FILE_SEQUENTIAL | PAUTHDBX dataset. |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | DFSVSMDB member in PROCLIB. |
| OEM.IMS.IMSP.RECON1 | IOType.FILE_SEQUENTIAL | RECON1 dataset. |
| OEM.IMS.IMSP.RECON2 | IOType.FILE_SEQUENTIAL | RECON2 dataset. |
| OEM.IMS.IMSP.RECON3 | IOType.FILE_SEQUENTIAL | RECON3 dataset. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 | IOType.FILE_SEQUENTIAL | Output dataset containing the unloaded DBD DBPAUTP0. |
| SYSPRINT | IOType.REPORT | System print output for messages and diagnostics. |
| SYSUDUMP | IOType.REPORT | System dump output for debugging purposes. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Deletes the output dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 if it exists. |
| DFSRRC00 | CallType.STATIC_CALL | Unloads the DBD DBPAUTP0. |

## Paragraphs/Procedures

### STEPDEL
This step deletes the output dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 before the unload process. It executes the utility program IEFBR14, which is a dummy program commonly used to delete datasets. The SYSPRINT DD statement directs the output messages to the system output. The SYSUT1 DD statement defines the dataset to be deleted, specifying its name, disposition (MOD,DELETE), unit (SYSDA), and space allocation (TRK,0). This ensures that any existing dataset with the same name is removed before the unload process begins.

### UNLOAD
This step executes the IMS program DFSRRC00 to unload the DBD DBPAUTP0. The PARM parameter specifies the unload utility (ULU), the unload control statement dataset (DFSURGU0), and the DBD name (DBPAUTP0). The STEPLIB DD statements define the libraries containing the necessary IMS modules. The DFSRESLB DD statement specifies the IMS RESLIB. The IMS DD statements define the PSBLIB and DBDLIB libraries. The DFSURGU1 DD statement defines the output dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0, specifying its disposition, unit, space allocation, and DCB attributes. The DDPAUTP0 and DDPAUTX0 DD statements define the PAUTHDB and PAUTHDBX datasets. The DFSVSAMP DD statement defines the DFSVSMDB member in the PROCLIB. The DFSCTL DD statement specifies the SBPARM ACTIV=COND parameter. The SYSUDUMP DD statement defines the system dump output. The RECON* DD statements define the RECON datasets. DFSWRK01 and DFSSRT01 are dummy datasets.

## Open Questions

- ? What is the purpose of the SBPARM ACTIV=COND parameter in the DFSCTL DD statement?
  - Context: The meaning and impact of this parameter are unclear from the JCL alone.
