# CBPAUP0J

**File**: `jcl/CBPAUP0J.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 15:10:36.568144

## Purpose

This JCL defines a batch job CBPAUP0J that executes the IMS region controller DFSRRC00 in BMP mode to run the application program CBPAUP0C using PSB PSBPAUTB. The job's purpose is to delete expired authorizations from an IMS database. It configures necessary IMS libraries, provides a control input via SYSIN, and routes outputs to SYSOUT datasets.

**Business Context**: Authorization system maintenance, purging expired records to ensure data currency and compliance.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PARM | IOType.PARAMETER | Execution parameters specifying BMP mode, application program CBPAUP0C, and PSB PSBPAUTB |
| SYSIN | IOType.OTHER | Single control card '00,00001,00001,Y' providing IMS transaction input parameters |
| STEPLIB | IOType.OTHER | Load libraries containing DFSRRC00 and CBPAUP0C |
| IMS | IOType.IMS_SEGMENT | PSB and DBD libraries for PSBPAUTB definition and database access |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSOUT* | IOType.REPORT | Various program outputs including standard print, abend aids, dumps, and IMS errors |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS batch region controller to invoke the application program |
| CBPAUP0C | CallType.DYNAMIC_CALL | Performs deletion of expired authorizations using IMS DL/I calls via PSBPAUTB |

## Business Rules

- **BR001**: Invoke IMS BMP to delete expired authorizations via program CBPAUP0C

## Paragraphs/Procedures

### STEP01
STEP01 is the single and primary execution step in this JCL job, serving as the orchestration point for running the IMS batch deletion process. It consumes the PARM parameter from line 25, which instructs DFSRRC00 to operate in BMP mode, load the application program CBPAUP0C, and use the PSB named PSBPAUTB for database access. Additionally, it reads the SYSIN dataset defined on lines 36-38 containing the control card '00,00001,00001,Y', which provides the formatted input transaction data for the IMS application, likely specifying PCB mask, message length, and processing flags. The step relies on input libraries specified in DD statements such as STEPLIB (lines 26-27 for RESLIB and LOADLIB), DFSRESLB (28), PROCLIB (29), DFSSEL (31), and IMS (33-34 for PSBLIB and DBDLIB), which supply the executable modules, procedures, and metadata for IMS databases. It produces outputs directed to multiple SYSOUT-class DD statements including SYSOUX (39), SYSOUT (40), SYSABOUT (41), ABENDAID (42), SYSPRINT (45), SYSUDUMP (46), and IMSERR (47), capturing normal program output, diagnostics, abend details, system dumps, and error information. Dummy datasets IEFRDER (43) and IMSLOGR (44) are used to discard unnecessary reader and log outputs. There is no conditional business logic or decision-making within the JCL step itself, as JCL steps execute unconditionally unless prior steps fail. Error handling is provided implicitly through the SYSOUT DD allocations for abend analysis and dumps, enabling post-execution diagnostics. The step calls DFSRRC00 statically via the EXEC PGM=, which dynamically invokes CBPAUP0C to perform the core logic of identifying and deleting expired authorization records from IMS databases defined in PSBPAUTB.

## Open Questions

- ? Precise interpretation of SYSIN control card fields '00,00001,00001,Y'
  - Context: Format specific to IMS DFSRRC00 batch transaction input; exact field meanings (e.g., PCB mask, segment hierarchy, flags) not deducible from JCL alone
- ? Specific IMS databases and segments accessed by PSBPAUTB
  - Context: PSBLIB/DBDLIB referenced but no PCB or DBD details in JCL
