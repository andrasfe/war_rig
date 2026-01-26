# DBPAUTP0

**File**: `jcl/DBPAUTP0.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 17:36:37.918022

## Purpose

This JCL defines a batch job that first deletes any existing output dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 to ensure a clean slate. It then executes the IMS Database Unload utility DFSRRC00 to unload the entire contents of the IMS database DBPAUTP0 using the PSB DFSURGU0. The unloaded data is written to a new sequential dataset with VB records.

**Business Context**: Supports CardDemo application by unloading IMS database DBPAUTP0, likely for backup, data migration, or demo data extraction (version note at line 46)

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS database serving as the source for unloading all segments |
| DDPAUTP0 | IOType.OTHER | PSB library containing DFSURGU0 application program for unload control |
| DDPAUTX0 | IOType.OTHER | Extended PSB library for DFSURGU0 |
| PARM | IOType.PARAMETER | Parameters directing unload utility: ULU (unload), DFSURGU0 (PSB), DBPAUTP0 (DBD) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 | IOType.FILE_SEQUENTIAL | Sequential dataset receiving the unloaded IMS database segments in DFSURGU0 format (VB, LRECL=27990) |
| SYSPRINT | IOType.REPORT | Utility messages, statistics, and diagnostics from DFSRRC00 |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Delete prior output dataset to prevent catalog conflicts |
| DFSRRC00 | CallType.STATIC_CALL | Execute IMS database unload utility for DBPAUTP0 |

## Business Rules

- **BR001**: Pre-delete output dataset before unload to ensure fresh cataloged output
- **BR002**: Use conditional segment activation during unload

## Paragraphs/Procedures

### STEPDEL
This step serves as a cleanup routine to delete any existing version of the unload output dataset before the main processing begins, preventing JCL errors from duplicate datasets. It consumes no input data streams but targets the dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 via SYSUT1 DD with DISP=(MOD,DELETE). The output is the deletion effect on the catalog, with SYSPRINT routed to SYSOUT for any messages. There is no business logic or conditions checked, as IEFBR14 is a dummy utility that only acts on DISP parameters. No validation or error handling beyond standard JCL return codes; if deletion fails (e.g., dataset not found), the job continues. This step is invoked directly by the JOB statement and calls no subordinate steps or programs. Its role ensures idempotency for repeated job submissions.

### UNLOAD
This is the primary processing step that orchestrates the unloading of the entire IMS database DBPAUTP0 using the DFSRRC00 utility program. It consumes input from the IMS database DBPAUTP0 (specified in PARM), PSB libraries (DDPAUTP0, DDPAUTX0), DBD libraries (IMS DD), and control parameters via DFSCTL including SBPARM ACTIV=COND for conditional dependent segment processing. Outputs are produced to DFSURGU1 (main unload file with SPACE and DCB specs for large VB records), SYSPRINT for logs, and SYSUDUMP for abends. Business logic follows standard IMS unload rules: starts at root segments, unloads them entirely, and conditionally unloads dependents based on PSB DFSURGU0 logic and ACTIV=COND. No explicit validations in JCL, but utility handles I/O errors internally. Error conditions trigger SYSUDUMP with detailed dump. Dummy DDs (DFSWRK01, DFSSRT01) indicate no sort or work files needed. RECON DDs provide reconciliation libraries. This step is called sequentially after STEPDEL by the JOB and invokes no sub-steps.
