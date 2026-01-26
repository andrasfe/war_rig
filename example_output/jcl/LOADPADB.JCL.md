# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: JCL
**Analyzed**: 2026-01-26 17:37:00.972859

## Purpose

This JCL defines a batch job to execute the IMS batch DL/I driver program DFSRRC00 using PARM='BMP,PAUDBLOD,PSBPAUTB' for loading the PAUTDB IMS database. It supplies sequential input files for root and child segments from prior unloads, IMS libraries, and utility control cards. Standard output datasets capture prints, dumps, and errors.

**Business Context**: Supports the AWS M2 CardDemo application by loading the Payment Authorization User Test Database (PAUTDB) with root and child segment data.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | FILE_SEQUENTIAL | Sequential unload file containing root segment data for PAUTDB database load |
| INFILE2 | FILE_SEQUENTIAL | Sequential unload file containing child segment data for PAUTDB database load |
| DFSVSAMP | OTHER | Dataset containing IMS utility control statements (member DFSVSMDB) directing the load operation |
| STEPLIB | OTHER | Load libraries including IMS RESLIBs and application loadlib AWS.M2.CARDDEMO.LOADLIB |
| IMS | OTHER | PSBLIB and DBDLIB for PSBPAUTB and PAUDBLOD database definitions |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | REPORT | Standard print output from the IMS utility execution |
| SYSUDUMP | REPORT | System abend dump output |
| IMSERR | REPORT | IMS-specific error messages |
| PAUTHDB | IMS_SEGMENT | Target IMS database for loaded data (primary and index; DDs commented out at lines 40-41) |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | STATIC_CALL | Drives IMS batch DL/I processing for PAUTDB load using specified BMP mode, database load spec, and PSB |

## Business Rules

- **BR-JCL-001**: No application business rules present; JCL provides execution environment only

## Paragraphs/Procedures

### STEP01
STEP01 is the only execution step in this JCL job and serves as the primary orchestration point for running the IMS database load utility. Its main role is to invoke PGM=DFSRRC00 with PARM='BMP,PAUDBLOD,PSBPAUTB' to process input unload files into the PAUTDB IMS database using the PSBPAUTB definition. It consumes sequential data from INFILE1 (root segments, line 36) and INFILE2 (child segments, line 38), along with IMS libraries from STEPLIB (lines 28-30), PSBLIB/DBDLIB from IMS DD (lines 33-34), and control directives from DFSVSAMP/DFSVSMDB (lines 44-45). Outputs are generated to SYSPRINT (line 48) for logs/reports, SYSUDUMP (line 49) for diagnostics, and IMSERR (line 50) for errors; potential database outputs PAUTHDB/PAUTHDBX are referenced but commented out (lines 40-41), implying possible default IMS access or configuration-specific allocation. No explicit conditions or decisions are coded in the JCL, as processing flow is dictated by the utility's control cards and PARM values. Error handling relies on standard z/OS mechanisms, with DUMMY datasets (e.g., IMSLOGR line 46, IEFRDER line 47) suppressing unnecessary logs and directing dumps to SYSOUT. This step does not invoke subordinate steps, programs, or paragraphs, functioning as the terminal execution unit invoked by the JOB statement (line 1).

## Open Questions

- ? What is the precise function of PARM='BMP,PAUDBLOD,PSBPAUTB' in DFSRRC00?
  - Context: PARM format suggests BMP mode with DBD/PSB, but exact mapping unclear without IMS utility docs
- ? What specific control statements define the utility action in DFSVSMDB member?
  - Context: DD references the member but content not included in JCL
- ? Purpose of commenting out DDPAUTP0 and DDPAUTX0?
  - Context: These appear to be target VSAM datasets for PAUTHDB load but disabled
