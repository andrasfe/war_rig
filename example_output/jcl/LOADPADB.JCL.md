# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 02:32:45.871509

## Purpose

This JCL submits a batch job to execute the IMS utility program DFSRRC00 in BMP mode with PARM='BMP,PAUDBLOD,PSBPAUTB' to load an IMS database (likely PAUTHDB based on commented DDs). It reads root and child segment data from sequential unload files associated with PAUTDB and control statements from an external member. The job references IMS libraries for execution and produces standard print, dump, and error outputs.

**Business Context**: Supports AWS M2 Card Demo application by loading IMS database for payment authorization table (inferred from PAUTB PSB and dataset names).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | IOType.FILE_SEQUENTIAL | Root segment data from AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO |
| INFILE2 | IOType.FILE_SEQUENTIAL | Child segment data from AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO |
| DFSVSAMP | IOType.FILE_SEQUENTIAL | IMS utility control statements from OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB) |
| PARM | IOType.PARAMETER | 'BMP,PAUDBLOD,PSBPAUTB' defining BMP region, load program identifier, and PSB name |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard print output including job logs |
| SYSUDUMP | IOType.REPORT | System dump output on abends |
| IMSERR | IOType.REPORT | IMS-specific error messages |
| PAUTHDB | IOType.IMS_SEGMENT | Intended primary IMS database dataset for load (DDPAUTP0 commented out) |
| PAUTHDBX | IOType.IMS_SEGMENT | Intended index IMS database dataset for load (DDPAUTX0 commented out) |

## Business Rules

- **BR001**: Hierarchical segment loading into IMS database with root segments from INFILE1 and child segments from INFILE2

## Paragraphs/Procedures

### STEP01
This JCL step is the sole execution step in the job, invoking PGM=DFSRRC00 as an IMS utility in BMP mode to perform database loading. It consumes data inputs from INFILE1 (root segments at line 36), INFILE2 (child segments at line 38), and control statements from DFSVSAMP (lines 44-45), along with IMS libraries from STEPLIB (lines 28-30), PSBLIB (33), and DBDLIB (34). The step produces loaded data into the target IMS database PAUTHDB/PAUTHDBX (commented lines 40-41), plus report outputs to SYSPRINT (48), SYSUDUMP (49), and IMSERR (50). Business logic is delegated to the DFSRRC00 utility, which enforces IMS database rules like segment hierarchy, key uniqueness, and data type validation based on the DBD and control cards. No explicit decisions in JCL itself; conditions are handled by the utility (e.g., invalid data causes abend). Error handling relies on standard z/OS/IMS mechanisms, directing dumps to SYSUDUMP and errors to IMSERR. The step does not call other paragraphs or programs directly; it executes DFSRRC00, which internally processes PAUDBLOD logic via PARM (27). Overall, it orchestrates the environment for batch IMS database population without local validation.

## Open Questions

- ? Exact DBD name and load details (e.g., segment order, validation rules)
  - Context: Determined by PARM PAUDBLOD (27) and DFSVSMDB control member (45); naming between PAUTDB inputs and PAUTHDB outputs unclear
- ? Role of commented DDPAUTP0/DDPAUTX0
  - Context: Lines 40-41 commented; unclear if dynamically allocated or if load uses different DDs
