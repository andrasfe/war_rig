# CBPAUP0J

**File**: `jcl/CBPAUP0J.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-27 02:42:05.309734

## Purpose

This JCL job executes the IMS region controller DFSRRC00 in BMP mode to run the application program CBPAUP0C using PSB PSBPAUTB. The job's purpose is to delete expired authorizations as documented in the header comment. Input parameters and transaction data are provided via PARM and SYSIN.

**Business Context**: IMS-based authorization management system cleanup process for expired records

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PARM | IOType.PARAMETER | Execution parameters for DFSRRC00: 'BMP' (Batch Message Processing mode), 'CBPAUP0C' (application program name), 'PSBPAUTB' (PSB name for database access) |
| SYSIN | IOType.OTHER | Inline input data for IMS transaction processing: '00,00001,00001,Y' |
| IMS PSBLIB/DBDLIB | IOType.OTHER | IMS PSB and DBD libraries required for program execution and database navigation |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard job print output including IMS processing logs |
| SYSOUT/SYSOUX/SYSABOUT/ABENDAID/SYSUDUMP/IMSERR | IOType.REPORT | Various SYSOUT datasets capturing transaction output, abend aids, dumps, and error logs |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS region controller program to execute BMP application CBPAUP0C |
| CBPAUP0C | CallType.DYNAMIC_CALL | Application program performing deletion of expired authorizations |

## Business Rules

- **BR001**: Execute IMS program to delete expired authorizations

## Paragraphs/Procedures

### STEP01
This is the sole execution step in the JCL job, serving as the main orchestration point for running the IMS BMP workload. Its primary purpose is to invoke DFSRRC00, the IMS control region program, to execute the application CBPAUP0C under PSBPAUTB for deleting expired authorizations. It consumes inputs from the PARM parameter on line 25 ('BMP,CBPAUP0C,PSBPAUTB') specifying the mode, program, and PSB, and from SYSIN on lines 36-37 providing transaction data '00,00001,00001,Y'. It also relies on library datasets defined in STEPLIB (lines 26-27), DFSRESLB (28), PROCLIB (29), DFSSEL (31), and IMS (33-34) for IMS runtime support, PSBs, and DBDs. Outputs are produced to multiple SYSOUT-class DD statements (lines 39-47) including SYSPRINT for logs, SYSUDUMP/ABENDAID for diagnostics, and others for IMS-specific traces. There are no conditional business decisions implemented directly in the JCL; all logic, including any validations or deletions, is handled by the called IMS program CBPAUP0C. Error handling is provided implicitly via ABENDAID (line 42) and SYSUDUMP (line 46) for capturing program abends and system dumps, with dummy DD's like IEFRDER (43) and IMSLOGR (44) suppressing unnecessary files. The step calls DFSRRC00 statically via EXEC PGM=, which in turn dynamically processes CBPAUP0C based on PARM. Upon completion, standard job return codes are set based on the IMS program's outcome.

## Open Questions

- ? What specific meaning do the SYSIN values '00,00001,00001,Y' hold?
  - Context: Values are provided inline but not explained in JCL comments
- ? What IMS databases/segments are accessed via PSBPAUTB?
  - Context: PSB name given but no DBD or segment details in JCL
