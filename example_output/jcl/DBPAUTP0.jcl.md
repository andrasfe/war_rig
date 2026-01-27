# DBPAUTP0

**File**: `jcl/DBPAUTP0.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-27 23:06:24.734816

## Purpose

This JCL job unloads the DBD (Database Description) for DBPAUTP0. It first deletes the output dataset if it exists, then executes the IMS program DFSRRC00 to perform the unload, and finally catalogs the output dataset.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSB Library |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBD Library |
| OEM.IMS.IMSP.PAUTHDB | IOType.FILE_SEQUENTIAL | IMS PAUTHDB |
| OEM.IMS.IMSP.PAUTHDBX | IOType.FILE_SEQUENTIAL | IMS PAUTHDBX |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | DFSVSMDB member from the OEMPP.IMS.V15R01MB.PROCLIB |
| OEM.IMS.IMSP.RECON1 | IOType.FILE_SEQUENTIAL | IMS RECON1 dataset |
| OEM.IMS.IMSP.RECON2 | IOType.FILE_SEQUENTIAL | IMS RECON2 dataset |
| OEM.IMS.IMSP.RECON3 | IOType.FILE_SEQUENTIAL | IMS RECON3 dataset |
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS SDFSRESL |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Load library |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 | IOType.FILE_SEQUENTIAL | Output dataset containing the unloaded DBD data for DBPAUTP0. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Deletes the output dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 if it exists. |
| DFSRRC00 | CallType.STATIC_CALL | Unloads the DBD for DBPAUTP0 using the ULU function and DFSURGU0 parameters. |

## Business Rules

- **BR001**: Specifies conditional activation using SBPARM.

## Paragraphs/Procedures

### STEPDEL
This step deletes the output dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 if it already exists. It uses the IEFBR14 program, a standard IBM utility for dataset management. The SYSPRINT DD statement directs the output of the deletion process to the job log. The SYSUT1 DD statement defines the dataset to be deleted, specifying its name, disposition (MOD,DELETE), unit (SYSDA), and space allocation (TRK,0). This step ensures that the subsequent unload process can create a new dataset without encountering a pre-existing dataset error. It consumes the dataset name and disposition parameters from the SYSUT1 DD statement. It produces no direct output other than messages to the job log. It does not call any other programs or paragraphs.

### ~~UNLOAD~~ (Dead Code)
*Paragraph 'UNLOAD' is never PERFORMed or referenced by any other paragraph or program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| UNLOAD | paragraph | 15 | Paragraph 'UNLOAD' is never PERFORMed or referenced by any other paragraph or program |
