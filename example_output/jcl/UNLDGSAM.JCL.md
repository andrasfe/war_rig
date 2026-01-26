# UNLDGSAM

**File**: `jcl/UNLDGSAM.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 14:25:04.182555

## Purpose

JCL to execute the IMS Database Unload utility (DBUNLDGS) via DFSRRC00 program in DLI mode for the DLIGSAMP database. It reads ROOT and CHILD GSAM input files (PASFILOP and PADFILOP) containing database segments. Control statements from DFSVSAMP define the unload process and output datasets.

**Business Context**: Supports unloading of PAUTDB IMS database segments in the AWS M2 CARDDEMO application, likely for backup, migration, or analysis.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.FILE_SEQUENTIAL | GSAM dataset containing ROOT segments of PAUTDB database |
| PADFILOP | IOType.FILE_SEQUENTIAL | GSAM dataset containing CHILD segments of PAUTDB database |
| DFSVSAMP | IOType.FILE_SEQUENTIAL | Control statements for DBUNLDGS utility from PROCLIB(DFSVSMDB), defining unload parameters and output datasets |
| PARM | IOType.PARAMETER | Utility parameters 'DLI,DBUNLDGS,DLIGSAMP,,,,,,,,,,,N' specifying DLI access, DBUNLDGS utility, and DLIGSAMP database |
| DDPAUTP0 | IOType.OTHER | IMS PAUTHDB library for database authorization or definitions |
| DDPAUTX0 | IOType.OTHER | IMS PAUTHDBX library for extended database authorization or definitions |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| UNLOAD-DATASETS | IOType.FILE_SEQUENTIAL | Sequential datasets containing unloaded IMS database segments (ROOT and CHILD), as defined in DFSVSAMP control statements |
| SYSPRINT | IOType.REPORT | Standard utility print output including messages and reports |
| IMSERR | IOType.REPORT | IMS-specific error messages |
| SYSUDUMP | IOType.REPORT | System dump output on program abend |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS general utility driver executing DBUNLDGS for database unload |

## Paragraphs/Procedures

### STEP01
This is the only step in the JCL, serving as the primary execution point for the IMS Database Unload utility. Its main role is to invoke DFSRRC00 with parameters for DBUNLDGS on the DLIGSAMP database in DLI mode, orchestrating the unload process. It consumes input datasets PASFILOP (ROOT GSAM) and PADFILOP (CHILD GSAM) containing the IMS database segments to be unloaded, along with control statements from DFSVSAMP (PROCLIB DFSVSMDB) that specify unload options, segment selection, and output dataset definitions. Additional inputs include IMS libraries (STEPLIB, DFSRESLB, IMS/PSBLIB/DBDLIB) and PAUTHDB libraries for runtime support. It produces output unload sequential files as defined in the control statements, capturing the raw database segments, and directs reports/errors to SYSPRINT, IMSERR, and SYSUDUMP. There are no conditional decisions or business rules in the JCL; all processing logic is delegated to the IMS utility based on PARM and control cards. Error handling relies on standard JCL/IMS mechanisms: abnormal termination keeps input files (DISP=OLD,KEEP,KEEP), dumps to SYSUDUMP, and errors to IMSERR. No subordinate JCL steps or paragraphs are called; it statically executes PGM=DFSRRC00 to perform the unload. The step ensures all required libraries are accessible via concatenated DD statements. Upon completion, the job notifies the submitter (NOTIFY=&SYSUID).

## Open Questions

- ? What are the exact contents of the DFSVSAMP control statements in PROCLIB(DFSVSMDB)?
  - Context: Referenced as input but source code does not include the member contents; needed for precise output dataset names and unload options
- ? Relationship between PAUTDB DSNs and DLIGSAMP database name?
  - Context: PARM specifies DLIGSAMP but inputs are PAUTDB.ROOT/CHILD.GSAM; likely aliases or specific implementation
