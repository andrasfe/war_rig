# UNLDGSAM

**File**: `jcl/UNLDGSAM.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 17:39:39.337207

## Purpose

This JCL defines a batch job that executes the IMS utility DFSRRC00 to perform an unload operation on GSAM databases within the PAUTDB structure. It targets the ROOT.GSAM and CHILD.GSAM datasets using DLI access method, with specific database and DBD names provided in the PARM. The job references IMS libraries and control files for the unload process.

**Business Context**: Supports the AWS M2 CARDDEMO application by unloading IMS PAUTDB GSAM databases for backup, migration, or analysis purposes.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.IMS_SEGMENT | GSAM database file for PAUTDB.ROOT containing root segments to be unloaded |
| PADFILOP | IOType.IMS_SEGMENT | GSAM database file for PAUTDB.CHILD containing child segments to be unloaded |
| STEPLIB | IOType.OTHER | Load libraries for IMS SDFSRESL and application LOADLIB |
| PARM | IOType.PARAMETER | Execution parameters for DFSRRC00: 'DLI,DBUNLDGS,DLIGSAMP,,,,,,,,,,,N' specifying DLI access, database DBUNLDGS, DBD DLIGSAMP, and no VSAM output |
| DFSRESLB | IOType.OTHER | IMS resource library |
| IMS | IOType.OTHER | IMS PSBLIB and DBDLIB for database definitions |
| DDPAUTP0 | IOType.OTHER | IMS PAUTHDB for authorization |
| DDPAUTX0 | IOType.OTHER | IMS PAUTHDBX for extended authorization |
| DFSVSAMP | IOType.OTHER | IMS proclib member DFSVSMDB for VSAM definitions |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard print output including utility messages and logs |
| SYSUDUMP | IOType.REPORT | System dump output for abend diagnostics |
| IMSERR | IOType.REPORT | IMS error log output |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS general utility to unload GSAM database segments from PAUTDB ROOT and CHILD |

## Business Rules

- **BR001**: IMS unload utility uses DLI access method with database name DBUNLDGS and DBD name DLIGSAMP
- **BR002**: Input GSAM datasets use DISP=(OLD,KEEP,KEEP) to ensure exclusive access while preserving datasets post-job
- **BR003**: No VSAM output file generated (indicated by trailing 'N' in PARM)
- **BR004**: Job uses multiple IMS libraries for resolution and authorization

## Paragraphs/Procedures

### STEP01
This is the sole execution step in the JCL job, serving as the primary orchestration point for running the IMS database unload utility. It consumes input from multiple DD datasets including PASFILOP (PAUTDB.ROOT.GSAM), PADFILOP (PAUTDB.CHILD.GSAM), STEPLIB libraries (OEMA.IMS.IMSP.SDFSRESL and AWS.M2.CARDDEMO.LOADLIB), IMS control libraries (PSBLIB, DBDLIB), authorization datasets (PAUTHDB, PAUTHDBX), and DFSVSAMP proclib. The step produces output to SYSPRINT, SYSUDUMP, and IMSERR for logs, dumps, and errors. Business logic is minimal as it is JCL, but it enforces rules via PARM='DLI,DBUNLDGS,DLIGSAMP,,,,,,,,,,,N' specifying DLI access, database unload for DBUNLDGS with DBD DLIGSAMP, and no VSAM output. Dataset dispositions ensure inputs are opened exclusively (OLD) but kept post-execution (KEEP,KEEP). Error handling relies on SYSUDUMP for abends and DUMMY DDs (IMSLOGR, IEFRDER) to suppress unnecessary logging. It statically calls DFSRRC00 to perform the actual unload of GSAM segments from ROOT and CHILD databases. No subordinate paragraphs exist as this is JCL, not COBOL/PL/I. The step runs under job parameters like CLASS=A, REGION=0M, TIME=1440 for resource allocation.

## Open Questions

- ? Exact mapping of DBUNLDGS database to PAUTDB ROOT/CHILD GSAM files
  - Context: PARM references DBUNLDGS and DLIGSAMP, but DD names suggest PAUTDB; no explicit link in JCL
- ? Presence of output unload file
  - Context: No explicit DD for unload output; PARM ends with 'N' suggesting no VSAM output, but typical unload produces sequential file
