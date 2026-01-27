# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-27 23:05:57.417285

## Purpose

This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database. It specifies the program, parameters, and required datasets for the IMS BMP region.

**Business Context**: UNKNOWN

## Inputs

| Name | Type | Description |
|------|------|-------------|
| OEMA.IMS.IMSP.SDFSRESL | IOType.FILE_SEQUENTIAL | IMS RESLIB |
| AWS.M2.CARDDEMO.LOADLIB | IOType.FILE_SEQUENTIAL | Application load library |
| OEM.IMS.IMSP.PSBLIB | IOType.FILE_SEQUENTIAL | IMS PSB Library |
| OEM.IMS.IMSP.DBDLIB | IOType.FILE_SEQUENTIAL | IMS DBD Library |
| AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO | IOType.FILE_SEQUENTIAL | Input file for PAUTDB root segment |
| AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO | IOType.FILE_SEQUENTIAL | Input file for PAUTDB child segment |
| OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) | IOType.FILE_SEQUENTIAL | IMS VSAM Definition Parameters |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | System print output |
| SYSUDUMP | IOType.REPORT | System dump output |
| IMSERR | IOType.REPORT | IMS error output |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | Executes the IMS program to load the PAUTDB database. |

## Paragraphs/Procedures

### STEP01
This step executes the IMS program DFSRRC00, which is responsible for loading the PAUTDB database. The PARM parameter specifies the execution environment as a BMP region, the database load utility (PAUDBLOD), and the PSB name (PSBPAUTB). The STEPLIB DD statements define the libraries containing the IMS RESLIB and application load modules required for execution. The DFSRESLB DD statement specifies the IMS RESLIB. The IMS DD statement defines the PSBLIB and DBDLIB datasets. INFILE1 and INFILE2 DD statements define the input datasets for the PAUTDB root and child segments, respectively. DFSVSAMP defines the VSAM parameters. Finally, SYSPRINT, SYSUDUMP, and IMSERR DD statements define the output datasets for system messages, dumps, and IMS error messages.

## Open Questions

- ? What is the business context for loading the PAUTDB database?
  - Context: The JCL does not provide information about the business purpose of this database.
