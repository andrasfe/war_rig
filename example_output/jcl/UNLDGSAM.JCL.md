# UNLDGSAM

**File**: `jcl/UNLDGSAM.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-28 14:55:41.670823

## Purpose

This JCL job executes the IMS program DFSRRC00 to unload a GSAM database. It specifies the program parameters, required libraries, and input/output datasets for the IMS database unload process.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.PAUTDB.ROOT.GSAM | IOType.FILE_SEQUENTIAL | Input GSAM database root segment to be unloaded. |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.GSAM | IOType.FILE_SEQUENTIAL | Input GSAM database child segment to be unloaded. |
| OEM.IMS.IMSP.PAUTHDB | IOType.FILE_SEQUENTIAL | Input IMS PAUTHDB dataset. |
| OEM.IMS.IMSP.PAUTHDBX | IOType.FILE_SEQUENTIAL | Input IMS PAUTHDBX dataset. |
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB |
| OEMA.IMS.IMSP.SDFSRESL.V151 | IOType.FILE_SEQUENTIAL | IMS RESLIB version 151 |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Load library for the application |
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSBLIB |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBDLIB |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | IMS VSAM definition |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | System print output for the job. |
| SYSUDUMP | IOType.REPORT | System dump output for the job. |
| IMSERR | IOType.REPORT | IMS error output for the job. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | Executes the IMS DL/I batch utility to unload the GSAM database. |

## Paragraphs/Procedures

### STEP01
This step executes the IMS program DFSRRC00, which is the DL/I batch utility used to unload a GSAM database. The PARM parameter specifies the execution parameters for the IMS program, including the database unload function (DBUNLDGS) and the DLIGSAMP control region. The STEPLIB DD statements define the libraries required to execute the IMS program, including the IMS RESLIB and the application load library. The IMS DD statement defines the PSBLIB and DBDLIB datasets required for the IMS execution. The PASFILOP and PADFILOP DD statements define the input GSAM database datasets to be unloaded. The DFSVSAMP DD statement specifies the VSAM buffer pool parameters. The SYSPRINT, SYSUDUMP, and IMSERR DD statements define the output datasets for system messages, dumps, and IMS error messages, respectively. The IEFRDER and IMSLOGR DD statements are dummy datasets.

## Open Questions

- ? What is the exact format and content of the GSAM database being unloaded?
  - Context: The JCL provides the dataset names, but the specific data structure and fields are not defined here.
