# UNLDGSAM

**File**: `jcl/UNLDGSAM.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-03-16 20:03:05.437029

## Purpose

This JCL job executes the IMS program DFSRRC00 to unload GSAM databases. It specifies the program to execute, the parameters for the execution, and the datasets required for the IMS environment and the GSAM databases to be unloaded.

**Business Context**: UNKNOWN

## Inputs

| Name | Type | Description |
|------|------|-------------|
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB dataset containing IMS modules. |
| OEMA.IMS.IMSP.SDFSRESL.V151 | IOType.FILE_SEQUENTIAL | IMS RESLIB dataset containing IMS modules (versioned). |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Load library containing program modules. |
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSB library. |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBD library. |
| AWS.M2.CARDDEMO.PAUTDB.ROOT.GSAM | IOType.FILE_VSAM | Input GSAM database root segment to be unloaded. |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.GSAM | IOType.FILE_VSAM | Input GSAM database child segment to be unloaded. |
| OEM.IMS.IMSP.PAUTHDB | IOType.FILE_SEQUENTIAL | IMS PAUTHDB dataset. |
| OEM.IMS.IMSP.PAUTHDBX | IOType.FILE_SEQUENTIAL | IMS PAUTHDBX dataset. |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | IMS VSAM Definition Block PROCLIB member. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | System print output for job execution messages. |
| SYSUDUMP | IOType.REPORT | System dump output for debugging purposes. |
| IMSERR | IOType.REPORT | IMS error output. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | Executes the IMS DL/I program to unload the GSAM database. |

## Paragraphs/Procedures

### UNLDGSAM
This JCL job defines the overall job structure and attributes for unloading a GSAM database using IMS. It sets job-level parameters such as the job name, class, message class, message level, region size, notification ID, and time limit. The job's primary purpose is to execute the IMS program DFSRRC00 with specific parameters to unload the GSAM database. It does not directly process data but rather orchestrates the execution of the IMS program. The job defines the datasets required for the IMS environment and the GSAM databases to be unloaded. It does not perform any explicit error handling within the JCL itself; error handling is assumed to be managed by the called IMS program. The job executes the STEP01 step, which in turn executes the DFSRRC00 program.

### STEP01
This JCL step executes the IMS program DFSRRC00 with the specified parameters to unload a GSAM database. It defines the program to be executed (DFSRRC00) and the parameters passed to it, including 'DLI,DBUNLDGS,DLIGSAMP,,,,,,,,,,,N'. These parameters configure the IMS environment and specify the database unload function. The step defines the STEPLIB DD statements, which specify the libraries containing the IMS modules required for execution. It also defines the DFSRESLB DD statement, which points to the IMS RESLIB dataset. The IMS DD statement specifies the PSB and DBD libraries. The PASFILOP and PADFILOP DD statements define the input GSAM database datasets to be unloaded. The DDPAUTP0 and DDPAUTX0 DD statements define the IMS PAUTHDB and PAUTHDBX datasets. The DFSVSAMP DD statement defines the IMS VSAM Definition Block PROCLIB member. The IMSLOGR and IEFRDER DD statements are dummy datasets. Finally, the SYSPRINT, SYSUDUMP, and IMSERR DD statements define the output datasets for job execution messages, system dumps, and IMS errors, respectively. This step does not perform any explicit error handling within the JCL itself; error handling is assumed to be managed by the called IMS program DFSRRC00.

## Open Questions

- ? What is the business context for unloading this GSAM database?
  - Context: The JCL does not provide information about the specific business process that requires this database unload.
