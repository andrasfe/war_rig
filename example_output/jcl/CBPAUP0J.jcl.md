# CBPAUP0J

**File**: `jcl/CBPAUP0J.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 02:32:24.666348

## Purpose

This JCL defines a batch job that executes the IMS region controller DFSRRC00 to run the application program CBPAUP0C as a BMP under PSB PSBPAUTB. The program deletes expired authorizations from an IMS database. Standard IMS libraries and SYSOUT datasets are provided for execution, logging, and dumps.

**Business Context**: CARDDEMO system authorization management, deleting expired authorizations from IMS database

## Inputs

| Name | Type | Description |
|------|------|-------------|
| SYSIN | IOType.PARAMETER | IMS control statement '00,00001,00001,Y' providing parameters such as PCB index, position, length, and flag for the application program |
| PSBPAUTB | IOType.IMS_SEGMENT | IMS database segments read by CBPAUP0C to identify expired authorizations |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard IMS and program print output |
| SYSUDUMP | IOType.REPORT | System dump on abend |
| PSBPAUTB | IOType.IMS_SEGMENT | IMS database segments updated by deleting expired authorizations |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS region controller to execute CBPAUP0C as BMP for deleting expired authorizations |
| CBPAUP0C | CallType.STATIC_CALL | Application program that performs deletion of expired authorizations |

## Business Rules

- **BR001**: Delete expired authorizations from IMS database

## Paragraphs/Procedures

### STEP01
This is the only step in the JCL job, serving as the primary execution point for the IMS batch processing. It executes PGM=DFSRRC00 with PARM='BMP,CBPAUP0C,PSBPAUTB', consuming IMS libraries from STEPLIB (lines 26-27: IMS.SDFSRESL, XXXXXXXXX.PROD.LOADLIB), DFSRESLB (28), PROCLIB (29), DFSSEL (31), and IMS PSBLIB/DBDLIB (33-34). It reads control data from SYSIN (lines 36-37: '00,00001,00001,Y') to configure the IMS region and PCB for CBPAUP0C. The step produces outputs to multiple SYSOUT datasets including SYSOUX (39), SYSOUT (40), SYSABOUT (41), ABENDAID (42), SYSPRINT (45), SYSUDUMP (46), and IMSERR (47) for reports, logs, and abend diagnostics. Business logic is delegated to CBPAUP0C, which deletes expired authorizations based on PSBPAUTB database access; no decisions are made in JCL itself. Error handling relies on IMS standard mechanisms: abends produce dumps to SYSUDUMP and ABENDAID, with DUMMY DDs for IEFRDER (43) and IMSLOGR (44). It statically calls DFSRRC00, which in turn runs CBPAUP0C to perform the core deletion logic. Upon step completion, the job terminates with the IMS program's return code. No loops or validations occur in the JCL; all processing flow is controlled by the IMS region controller.

## Open Questions

- ? Precise format and meaning of SYSIN '00,00001,00001,Y'
  - Context: Standard IMS DFSRRC00 control card format not definitively identifiable from JCL alone
- ? Specific IMS segments and fields in PSBPAUTB accessed by CBPAUP0C
  - Context: JCL does not specify database details; defined in PSB and application program
