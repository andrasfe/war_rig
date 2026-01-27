# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-27 02:42:21.192266

## Purpose

This JCL job executes DFSRRC00 in BMP mode to invoke the PAUDBLOD IMS application program using PSB PSBPAUTB for loading the PAUDBLOD IMS hierarchical database. It reads root segments from INFILE1 and child segments from INFILE2 sequential unload files produced by prior jobs. The load process populates the database while directing reports and errors to SYSOUT datasets.

**Business Context**: Supports the AWS.M2.CARDDEMO application by reloading the PAUTDB IMS database after an unload operation.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | IOType.FILE_SEQUENTIAL | Sequential file containing root segments unloaded from PAUTDB database |
| INFILE2 | IOType.FILE_SEQUENTIAL | Sequential file containing child segments unloaded from PAUTDB database |
| IMS | IOType.OTHER | IMS PSBLIB and DBDLIB for PSB and DBD definitions |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUDBLOD | IOType.IMS_SEGMENT | IMS hierarchical database loaded with root and child segments from input files |
| SYSPRINT | IOType.REPORT | Standard print output from IMS execution |
| SYSUDUMP | IOType.REPORT | System dump output on abends |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS batch driver program to execute BMP application for database load |
| PAUDBLOD | CallType.DYNAMIC_CALL | Application program that performs the actual IMS database load from input files |

## Business Rules

- **BR001**: Root segments must be loaded before child segments to enforce hierarchical referential integrity in the IMS database
- **BR002**: Database load depends on successful prior unload jobs producing input files

## Paragraphs/Procedures

### STEP01
This is the only step in the JCL job, serving as the primary execution point for the IMS database load process. It consumes sequential input files INFILE1 for root segments and INFILE2 for child segments (lines 36,38), along with IMS libraries from STEPLIB (lines 28-30), DFSRESLB (31), IMS (33-34), and DFSVSAMP (44-45). The EXEC statement invokes PGM=DFSRRC00 with PARM='BMP,PAUDBLOD,PSBPAUTB' (lines 26-27), directing execution of the BMP application PAUDBLOD to insert data into the PAUDBLOD IMS database. Outputs include loaded database segments (implicit via application), reports to SYSPRINT (48), dumps to SYSUDUMP (49), and errors to IMSERR (50). There are no conditional decisions in the JCL; all business logic resides in the called PAUDBLOD program, which handles segment loading order and validation. Error handling relies on IMS-standard DDs: IMSLOGR DUMMY (46), SYSUDUMP, and IMSERR for diagnostics. No subordinate steps or procedures are invoked; control flows linearly through this single step. Commented DDs (40-41) suggest optional database access paths not used in this configuration.

## Open Questions

- ? What is the exact implementation logic in the PAUDBLOD application program?
  - Context: JCL provides execution environment but no source code for the BMP application
- ? Why are DDPAUTP0 and DDPAUTX0 commented out?
  - Context: Possible alternate database access paths not used; unclear if required for PAUDBLOD
