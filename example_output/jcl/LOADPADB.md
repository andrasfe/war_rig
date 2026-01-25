# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-25 18:30:19.047604

## Purpose

This JCL job executes the IMS reconstruction utility DFSRRC00 in BMP mode to load the PAUDB IMS database (DBD=PAUDBLOD) using PSB=PSBPAUTB. It reads sequential input files for root and child segments. Standard IMS DD statements and dumps are provided for execution.

**Business Context**: Supports loading data into PAUTDB IMS database for AWS M2 Card Demo application

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | IOType.FILE_SEQUENTIAL | Sequential file containing root segment data for PAUTDB database load |
| INFILE2 | IOType.FILE_SEQUENTIAL | Sequential file containing child segment data for PAUTDB database load |
| STEPLIB | IOType.OTHER | Load libraries for IMS SDFSRESL and application loadlib |
| IMS | IOType.OTHER | IMS PSB and DBD libraries |
| DFSVSAMP | IOType.FILE_SEQUENTIAL | IMS VSAM database definition member DFSVSMDB |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard print output from the utility execution |
| SYSUDUMP | IOType.REPORT | System dump output for abnormal terminations |
| IMSERR | IOType.REPORT | IMS error output |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS database reconstruction utility to load PAUDB database segments from input files |

## Paragraphs/Procedures

### STEP01
Executes IMS DFSRRC00 utility with PARM and DD allocations to load PAUDB database

## Open Questions

- ? Exact segment names and structures in INFILE1 and INFILE2
  - Context: JCL does not specify field-level details; would require examining the input files or DBD/PSB definitions
- ? Purpose of commented DDPAUTP0 and DDPAUTX0
  - Context: Lines 40-41 are commented out; unclear if they were previously used for database access
- ? Role of DFSVSAMP (DFSVSMDB)
  - Context: DD points to a PROCLIB member; exact usage in DFSRRC00 not determinable from JCL alone
