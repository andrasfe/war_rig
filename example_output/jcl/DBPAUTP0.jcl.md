# DBPAUTP0

**File**: `jcl/DBPAUTP0.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-27 02:39:57.178890

## Purpose

This JCL job deletes any existing IMS unload dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 using IEFBR14 in STEPDEL. It then executes the IMS unload utility DFSRRC00 in the UNLOAD step to unload the entire DBPAUTP0 database using user exit DFSURGU0, producing a sequential VB unload file in the same dataset. The job is part of the CardDemo application for database unload operations.

**Business Context**: Supports IMS database unloading for the CardDemo_v2.0 application, likely for backup, migration, or demo data preparation.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| SYSUT1 | IOType.FILE_SEQUENTIAL | Existing dataset targeted for deletion prior to unload |
| STEPLIB | IOType.OTHER | Load libraries containing IMS utilities and application modules |
| DFSRESLB | IOType.OTHER | IMS SDFSRESL resource library |
| IMS | IOType.OTHER | IMS PSBLIB and DBDLIB for program specification blocks and database definitions |
| DDPAUTP0 | IOType.FILE_SEQUENTIAL | Primary DBD library for database DBPAUTP0 |
| DDPAUTX0 | IOType.FILE_SEQUENTIAL | Secondary or index DBD library for database DBPAUTP0 |
| DFSVSAMP | IOType.OTHER | IMS sample VSAM macro database definition |
| DFSCTL | IOType.OTHER | Inline control statements including SBPARM ACTIV=COND for unload processing |
| RECON1 | IOType.OTHER | IMS reconciliation library 1 |
| RECON2 | IOType.OTHER | IMS reconciliation library 2 |
| RECON3 | IOType.OTHER | IMS reconciliation library 3 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Print output for both steps including messages and diagnostics |
| DFSURGU1 | IOType.FILE_SEQUENTIAL | Sequential VB unload file containing all data from IMS database DBPAUTP0 |
| SYSUDUMP | IOType.REPORT | System dump for abend diagnostics |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Delete pre-existing unload dataset to prepare for new unload |
| DFSRRC00 | CallType.STATIC_CALL | Perform IMS database unload for DBD DBPAUTP0 using specified user exit |

## Paragraphs/Procedures

### STEPDEL
The STEPDEL step serves as the initialization cleanup phase of the job, ensuring no residual output dataset exists from prior runs. It executes the z/OS utility IEFBR14, which handles dataset deletion without data processing. The sole input is the SYSUT1 DD statement referencing AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 with DISP=(MOD,DELETE), targeting deletion if the dataset exists. No data is read or processed; the operation is purely dispositional. It produces no persistent outputs, only directs any messages to SYSPRINT on SYSOUT=*. There is no business logic, validations, or conditional decisions implemented in this step. Error handling relies on standard JCL disposition and system abends if allocation fails. No subordinate steps, paragraphs, or programs are called from here. Control passes sequentially to the next step upon completion. This prevents overwrite issues or catalog conflicts in the subsequent UNLOAD step.

### UNLOAD
The UNLOAD step is the core processing phase, invoking the IMS Database Unload utility DFSRRC00 to extract all data from the DBPAUTP0 database. It consumes multiple input libraries and control files: STEPLIB/DFSRESLB/IMS for executables and definitions, DDPAUTP0/DDPAUTX0 for DBDs, DFSVSAMP for samples, DFSCTL for parameters like SBPARM ACTIV=COND, and RECON1-3 for reconciliation. The PARM=(ULU,DFSURGU0,DBPAUTP0) directs the utility to perform an unload (ULU) of DBD DBPAUTP0 using user exit DFSURGU0 for custom formatting. It reads all segments from the IMS database DBPAUTP0. Outputs include the primary DFSURGU1 sequential dataset (VB, LRECL=27990) containing unloaded records, SYSPRINT for logs, and SYSUDUMP for dumps. Dummy DDs (DFSWRK01, DFSSRT01) indicate no sort or work files needed. Business logic is encapsulated in the utility and user exit, with ACTIV=COND possibly restricting to active data. Errors trigger standard IMS abends with dumps to SYSUDUMP. No subordinate calls; it is the final processing step. Upon success, the job ends with the cataloged unload file ready for use.

## Open Questions

- ? What is the exact function and logic of the DFSURGU0 user exit?
  - Context: Specified in PARM=(ULU,DFSURGU0,DBPAUTP0) but no source or documentation provided in JCL.
- ? What specific behavior does SBPARM ACTIV=COND enforce in DFSRRC00?
  - Context: Inline DFSCTL parameter unclear without IMS utility manual reference.
- ? What is the record/segment structure of DBPAUTP0?
  - Context: DBD libraries referenced but no copybooks or DBD source provided.
