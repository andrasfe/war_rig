# DBPAUTP0

**File**: `jcl/DBPAUTP0.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 15:10:30.123190

## Purpose

This JCL defines a batch job that first deletes any existing output dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 and then executes the IMS unload utility DFSRRC00 to unload the entire contents of the IMS database DBD DBPAUTP0 into a new sequential VB dataset. The unload uses the user exit DFSURGU0 and conditional segment activation via SBPARM ACTIV=COND. It is part of the CardDemo application for IMS data management.

**Business Context**: Supports IMS database maintenance for the CardDemo application by providing an unload of DBPAUTP0 dataset, likely for backup, migration, or demo purposes.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS database (DBD) from which all segments are unloaded unconditionally using DFSRRC00 utility |
| DDPAUTP0 | IOType.FILE_SEQUENTIAL | IMS authorization database library OEM.IMS.IMSP.PAUTHDB required by the unload utility |
| DDPAUTX0 | IOType.FILE_SEQUENTIAL | IMS authorization database index OEM.IMS.IMSP.PAUTHDBX required by the unload utility |
| DFSCTL | IOType.PARAMETER | Inline control parameters for IMS utility including SBPARM ACTIV=COND for conditional segment activation |
| STEPLIB/DFSRESLB/IMS | IOType.OTHER | Load libraries and IMS resource libraries (SDFSRESL, LOADLIB, PSBLIB, DBDLIB) required to execute DFSRRC00 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| DFSURGU1 | IOType.FILE_SEQUENTIAL | Sequential VB dataset containing the unloaded IMS database segments from DBPAUTP0, cataloged for reuse |
| SYSUT1 (STEPDEL) | IOType.FILE_SEQUENTIAL | Existing dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 targeted for deletion prior to new unload |
| SYSPRINT | IOType.REPORT | Utility print output routed to SYSOUT=* for both steps |

## Business Rules

- **BR001**: Delete existing output dataset before unload to ensure clean slate
- **BR002**: Unload IMS database unconditionally with conditional segment activation

## Paragraphs/Procedures

### STEPDEL
This step serves as the cleanup phase of the job, ensuring no residual output dataset from prior executions interferes with the new unload. It executes the utility program IEFBR14, which performs dataset management operations. The primary input is the DD SYSUT1 pointing to AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 with DISP=(MOD,DELETE), reading catalog information to check existence. It produces the deletion of the dataset if present, preparing for the subsequent UNLOAD step. There is no complex business logic; it unconditionally attempts deletion per JCL disposition. No validations or conditions are checked beyond standard JCL catalog access. Error handling relies on JCL defaults: if deletion fails (e.g., not found), the step completes with RC=0; abends go to job-level handling. SYSPRINT captures any messages. This step does not call other paragraphs or programs but precedes UNLOAD in job flow. Its role ensures idempotency of the unload process.

### UNLOAD
This is the core processing step that performs the IMS database unload operation using DFSRRC00, the standard IMS utility for resource control. It consumes the IMS database DBPAUTP0 specified in PARM=(ULU,DFSURGU0,DBPAUTP0), reading all segments unconditionally (ULU) with the DFSURGU0 user routine for custom processing. Additional inputs include authorization libraries (DDPAUTP0, DDPAUTX0), control parameters (DFSCTL with SBPARM ACTIV=COND for conditional activation), and required IMS libraries via STEPLIB/DFSRESLB/IMS. It produces the output dataset DFSURGU1 as a new cataloged VB sequential file containing the unloaded segments. Business logic is driven by the utility: unconditional unload scans the database, applies conditional activation, and formats records per RECFM=VB LRECL=27990. No explicit decisions in JCL, but utility handles segment selection and errors internally. Error handling uses SYSUDUMP for dumps, RECON1-3 for recovery, and standard IMS return codes; JCL DUMMY for work/sort files avoids spills. SYSPRINT logs utility output. This step does not call subordinate paragraphs but relies on IMS internal processing. It follows STEPDEL and completes the job's primary objective of data unload.

## Open Questions

- ? Exact function of DFSURGU0 user routine
  - Context: PARM specifies it but source code not provided; assumed standard unload handler
- ? Specific segments unloaded under ACTIV=COND
  - Context: SBPARM ACTIV=COND implies conditional but no details in JCL
