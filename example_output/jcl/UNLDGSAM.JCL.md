# UNLDGSAM

**File**: `jcl/UNLDGSAM.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-27 02:42:19.932191

## Purpose

This JCL defines a batch job that executes the IMS utility DFSRRC00 to unload the GSAM database DBUNLDGS (using DBD DLIGSAMP) by reading root and child GSAM datasets from AWS.M2.CARDDEMO.PAUTDB. It configures necessary IMS libraries, database descriptor libraries, and dummy datasets for execution. Standard output datasets capture print, dumps, and errors.

**Business Context**: Supports AWS M2 CARDDEMO application by unloading PAUTDB IMS GSAM database, likely for backup, migration, or demo purposes.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PARM | IOType.PARAMETER | Command parameters for DFSRRC00 specifying DLI access mode, database name DBUNLDGS, DBD name DLIGSAMP, and other defaults for unload operation |
| STEPLIB | IOType.OTHER | Concatenated load libraries required for executing DFSRRC00 and related IMS modules |
| DFSRESLB | IOType.OTHER | IMS resource library for DFSRRC00 execution |
| IMS | IOType.OTHER | Concatenated IMS PSBLIB (parameter library) and DBDLIB (database definition library) for database metadata |
| PASFILOP | IOType.FILE_VSAM | GSAM dataset containing root segments of PAUTDB (DBUNLDGS database) |
| PADFILOP | IOType.FILE_VSAM | GSAM dataset containing child segments of PAUTDB (DBUNLDGS database) |
| DDPAUTP0 | IOType.OTHER | Primary IMS PAUTHDB library for database authorization and descriptors |
| DDPAUTX0 | IOType.OTHER | Secondary IMS PAUTHDBX library for database authorization and descriptors |
| DFSVSAMP | IOType.OTHER | IMS proclib member DFSVSMDB for system definitions |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard print output from DFSRRC00 utility execution |
| SYSUDUMP | IOType.REPORT | System dump output in case of abends |
| IMSERR | IOType.REPORT | IMS-specific error messages and logs |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS utility to unload/reorganize GSAM database DBUNLDGS using specified DBD and input GSAM files |

## Business Rules

- **BR001**: Retain input GSAM datasets after processing for safety or reuse
- **BR002**: Use DLI access mode for unload operation

## Paragraphs/Procedures

### STEP01
This JCL step is the sole execution step in the UNLDGSAM job, serving as the primary orchestration point for running the IMS DFSRRC00 utility. It consumes the PARM parameter specifying the unload operation for database DBUNLDGS using DBD DLIGSAMP in DLI mode (line 27), along with input GSAM datasets PASFILOP for root segments (lines 36-37) and PADFILOP for child segments (lines 39-40). It also reads control libraries including STEPLIB concatenations (lines 28-30), DFSRESLB (31), IMS PSBLIB/DBDLIB (33-34), DDPAUTP0/ DDPAUTX0 for auth/DBD (42-43), and DFSVSAMP proclib (46-47). Outputs are produced to SYSPRINT for listings (50), SYSUDUMP for abend dumps (51), and IMSERR for errors (52), with dummies for IMSLOGR (48) and IEFRDER (49). The business logic is delegated to DFSRRC00, which unloads the GSAM database based on the inputs; JCL enforces retention of input files via KEEP disposition. No explicit conditional execution or error branching is defined, relying on default JCL return code propagation from the utility. The step ensures all required IMS runtime libraries are available via concatenations. Upon completion, input datasets are kept for potential reuse or inspection. This step controls the entire program flow as there are no subsequent steps.

## Open Questions

- ? Does DFSRRC00 produce a specific sequential unload file, or is output only to SYSPRINT?
  - Context: No dedicated UNLOAD or output data DD is defined in the JCL
- ? Exact fields/segments in PASFILOP and PADFILOP
  - Context: JCL does not specify copybooks or layouts
