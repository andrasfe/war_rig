# DBPAUTP0

**File**: `jcl/DBPAUTP0.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-02-25 15:30:04.280679

## Purpose

This JCL job first deletes any existing dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 using IEFBR14 and then executes the IMS unload utility DFSRRC00 to unload the entire IMS database DBPAUTP0 into a sequential VB dataset at the same DSN. The unload uses user exit DFSURGU0 and conditional activation via SBPARM ACTIV=COND. It serves as a data extraction step for the CardDemo application.

**Business Context**: IMS database unload for CardDemo_v2.0 application, likely for backup, migration, or offline processing of DBPAUTP0 database.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | IMS database definition (DBD) for unloading all segments and data from DBPAUTP0 |
| DDPAUTP0 | IOType.OTHER | IMS PSB/DBD library containing DBPAUTP0 definition |
| DDPAUTX0 | IOType.OTHER | IMS extended DBD library for DBPAUTP0 |
| DFSVSAMP | IOType.OTHER | IMS sample VSAM DBD module for DBPAUTP0 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 | IOType.FILE_SEQUENTIAL | Sequential VB dataset containing unloaded IMS database records from DBPAUTP0 |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Delete existing output dataset to ensure clean creation |
| DFSRRC00 | CallType.STATIC_CALL | Perform IMS database unload (ULU) of DBPAUTP0 using exit DFSURGU0 |

## Paragraphs/Procedures

### STEPDEL
This step, named STEPDEL, serves as the primary purpose of conditionally deleting any pre-existing output dataset to prevent catalog conflicts or residual data in subsequent steps. It consumes the dataset name AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 specified in the SYSUT1 DD statement with DISP=(MOD,DELETE), which modifies and deletes if present. It produces no data outputs but directs SYSPRINT messages to SYSOUT=* for any utility feedback. There are no business logic decisions, validations, or conditional processing; it is a unconditional deletion utility execution. No explicit error handling is implemented beyond standard JCL disposition failure which would cause step abend. It executes the IEFBR14 program, a z/OS dummy utility specifically for dataset disposition without data movement. This step does not call any other steps, programs, or paragraphs. Upon successful completion, control flows to the next step UNLOAD. The step ensures the output DSN is fresh for the unload operation. SYSPRINT provides logging of the deletion activity.

### ~~UNLOAD~~ (Dead Code)
*Paragraph 'UNLOAD' is never PERFORMed or referenced by any other paragraph or program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| UNLOAD | paragraph | 15 | Paragraph 'UNLOAD' is never PERFORMed or referenced by any other paragraph or program |

## Sequence Diagram

```mermaid
sequenceDiagram
    participant STEPDEL as STEPDEL
    participant IEFBR14 as IEFBR14
    participant AWS_M2_CARDDEMO_IMSDATA_DBPAUTP0 as AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0
    participant UNLOAD as UNLOAD
    participant DFSRRC00 as DFSRRC00
    participant OEMA_IMS_IMSP_SDFSRESL as OEMA.IMS.IMSP.SDFSRESL
    participant AWS_M2_CARDDEMO_LOADLIB as AWS.M2.CARDDEMO.LOADLIB
    participant OEM_IMS_IMSP_PSBLIB as OEM.IMS.IMSP.PSBLIB
    participant OEM_IMS_IMSP_DBDLIB as OEM.IMS.IMSP.DBDLIB
    participant OEM_IMS_IMSP_PAUTHDB as OEM.IMS.IMSP.PAUTHDB
    participant OEM_IMS_IMSP_PAUTHDBX as OEM.IMS.IMSP.PAUTHDBX
    participant OEMPP_IMS_V15R01MB_PROCLIB as OEMPP.IMS.V15R01MB.PROCLIB
    participant OEM_IMS_IMSP_RECON1 as OEM.IMS.IMSP.RECON1
    participant OEM_IMS_IMSP_RECON2 as OEM.IMS.IMSP.RECON2
    participant OEM_IMS_IMSP_RECON3 as OEM.IMS.IMSP.RECON3
    STEPDEL->>IEFBR14: performs
    STEPDEL->>AWS_M2_CARDDEMO_IMSDATA_DBPAUTP0: performs
    UNLOAD->>DFSRRC00: performs
    UNLOAD->>OEMA_IMS_IMSP_SDFSRESL: performs
    UNLOAD->>AWS_M2_CARDDEMO_LOADLIB: performs
    UNLOAD->>OEMA_IMS_IMSP_SDFSRESL: performs
    UNLOAD->>OEM_IMS_IMSP_PSBLIB: performs
    UNLOAD->>OEM_IMS_IMSP_DBDLIB: performs
    UNLOAD->>AWS_M2_CARDDEMO_IMSDATA_DBPAUTP0: performs
    UNLOAD->>OEM_IMS_IMSP_PAUTHDB: performs
    UNLOAD->>OEM_IMS_IMSP_PAUTHDBX: performs
    UNLOAD->>OEMPP_IMS_V15R01MB_PROCLIB: performs
    UNLOAD->>OEM_IMS_IMSP_RECON1: performs
    UNLOAD->>OEM_IMS_IMSP_RECON2: performs
    UNLOAD->>OEM_IMS_IMSP_RECON3: performs
```
