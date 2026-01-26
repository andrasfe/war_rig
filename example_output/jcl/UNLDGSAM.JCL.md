# UNLDGSAM

**File**: `jcl/UNLDGSAM.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 15:13:59.576908

## Purpose

This JCL defines a batch job UNLDGSAM that executes the IMS utility DFSRRC00 to unload the GSAM database DLIGSAMP using the DBUNLDGS utility in DLI mode. It provides necessary STEPLIB, IMS libraries, input GSAM files (PAUTDB.ROOT.GSAM and PAUTDB.CHILD.GSAM), and shared authorization datasets. Standard output datasets capture print, dumps, and errors.

**Business Context**: Supports AWS M2 CardDemo application by unloading IMS GSAM databases, likely for backup, migration, or offline processing (inferred from dataset names AWS.M2.CARDDEMO.PAUTDB).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.FILE_VSAM | GSAM file for PAUTDB.ROOT, provided as input to IMS utility (DISP=OLD) |
| PADFILOP | IOType.FILE_VSAM | GSAM file for PAUTDB.CHILD, provided as input to IMS utility (DISP=OLD) |
| DDPAUTP0 | IOType.FILE_VSAM | IMS PAUTHDB dataset, shared reference likely for database authorization |
| DDPAUTX0 | IOType.FILE_VSAM | IMS PAUTHDBX dataset, shared reference likely for extended authorization |
| DFSVSAMP | IOType.OTHER | IMS proclib member defining VSAM database structure (DFSVSMDB) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard utility print output |
| SYSUDUMP | IOType.REPORT | System dump output for abends |
| IMSERR | IOType.REPORT | IMS-specific error messages |

## Business Rules

- **BR001**: Unloads specific GSAM database DLIGSAMP using IMS DBUNLDGS utility

## Paragraphs/Procedures

### STEP01
STEP01 serves as the primary and only execution step in this JCL job, orchestrating the invocation of the IMS database unload utility. Its core purpose is to execute PGM=DFSRRC00 with a specific PARM='DLI,DBUNLDGS,DLIGSAMP,,,,,,,,,,,N' to perform a GSAM database unload operation on DLIGSAMP using the DBUNLDGS utility module in DLI access mode. It consumes IMS libraries from STEPLIB (lines 28-30), IMS macro libraries (lines 33-34), input GSAM datasets PASFILOP and PADFILOP (lines 36-40), authorization datasets DDPAUTP0 and DDPAUTX0 (lines 42-43), and the VSAM database definition from DFSVSAMP (lines 46-47). These inputs provide the runtime environment, database files, and structural definitions required for the unload process. The step produces outputs to SYSPRINT for logs and reports (line 50), SYSUDUMP for any abend diagnostics (line 51), and IMSERR for utility-specific errors (line 52). Business logic is encapsulated in the PARM specifying the exact utility and target database, with no additional conditional processing at the JCL level. Error handling relies on IMS utility defaults, routing failures to SYSUDUMP and IMSERR DDs, while dummy DDs (lines 45,48,49) suppress unnecessary logging for IMSLOGR and IEFRDER. No subordinate paragraphs or programs are called from this step, as it directly EXECs DFSRRC00. The configuration ensures high availability via DISP=SHR for shared datasets and appropriate job parameters like REGION=0M and TIME=1440 (lines 1-2). Overall, this step handles the complete unload workflow from resource setup to output capture.

## Open Questions

- ? Role of separate PASFILOP (ROOT.GSAM) and PADFILOP (CHILD.GSAM) files relative to single DLIGSAMP database
  - Context: PARM specifies one database DLIGSAMP (line 27), but two GSAM input files suggest possible multi-file DB or additional data sources
- ? Exact mapping of PAUTDB GSAM files to DLIGSAMP database
  - Context: Dataset names PAUTDB.ROOT/CHILD.GSAM differ from PARM database name DLIGSAMP
