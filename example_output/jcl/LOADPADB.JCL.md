# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 15:10:40.103992

## Purpose

This JCL job executes a single batch step STEP01 running the IMS utility DFSRRC00 in BMP mode with application program PAUDBLOD and PSB PSBPAUTB to load data into an IMS database. It provides sequential input files INFILE1 (root segments) and INFILE2 (child segments) from the AWS.M2.CARDDEMO.PAUTDB datasets. Supporting IMS libraries and control files are referenced for execution.

**Business Context**: Loads authorization test database (PAUTDB/PAUTB) for AWS M2 Card Demo application

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | IOType.FILE_SEQUENTIAL | Sequential input file containing root segment data for PAUTDB load |
| INFILE2 | IOType.FILE_SEQUENTIAL | Sequential input file containing child segment data for PAUTDB load |
| STEPLIB | IOType.OTHER | Load libraries for IMS RESLIB and application modules |
| IMS | IOType.OTHER | PSBLIB and DBDLIB for IMS control blocks |
| DFSVSAMP | IOType.OTHER | IMS VSAM model database definition member |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard utility print output routed to SYSOUT=* |
| SYSUDUMP | IOType.REPORT | System abend dump output routed to SYSOUT=* |
| IMSERR | IOType.REPORT | IMS-specific error messages routed to SYSOUT=* |

## Paragraphs/Procedures

### STEP01
This is the only step in the LOADPADB JCL job, serving as the primary execution point for the IMS database load process. Its main role is to invoke the DFSRRC00 utility program under IMS BMP control to run the user application PAUDBLOD with PSB PSBPAUTB for database reconstruction/loading (line 26-27). It consumes sequential input files INFILE1 (root data) and INFILE2 (child data) specified as SHR datasets (lines 36,38), along with IMS libraries from STEPLIB (lines 28-30), DFSRESLB (31), IMS PSB/DBD libs (33-34), and DFSVSAMP model (44-45). Outputs produced include standard reports to SYSPRINT, dumps to SYSUDUMP, and errors to IMSERR, all routed to SYSOUT=* (lines 48-50); the primary database output (PAUTHDB/PAUTHDBX) is implied via IMS dynamic allocation but DDs are commented out (lines 40-41). No explicit business logic or decisions are implemented in the JCL itself, as control and validation reside in the PAUDBLOD program executed via PARM. Error handling relies on IMS environment defaults, with dumps captured in SYSUDUMP and IMSERR for diagnostics (lines 49-50); DUMMY DDs for IMSLOGR and IEFRDER prevent unnecessary allocations (lines 46-47). The step does not call any other JCL steps, programs, or paragraphs, operating as a standalone batch execution. Resource constraints are inherited from the JOB card, including TIME=1440, REGION=0M, and NOTIFY (lines 1-2). Commented database DDs suggest this JCL may be a template requiring uncommenting for production use. Overall, STEP01 orchestrates the environment for PAUDBLOD to process inputs into the PAUTDB IMS database.

## Open Questions

- ? What are the exact target IMS database datasets (e.g., PAUTHDB, PAUTHDBX)?
  - Context: DDPAUTP0 and DDPAUTX0 are commented out (lines 40-41); loading likely uses dynamic allocation via DBD referenced in IMS DD and PSBPAUTB.
- ? What is the detailed function of the PAUDBLOD application program?
  - Context: Referenced in PARM (line 27) but source not provided; handles segment loading logic.
- ? Does this JCL perform unload, load, or reconstruction?
  - Context: DFSRRC00 is reconstruction utility; inputs suggest load, but confirmed only by naming (PAUDBLOD).
