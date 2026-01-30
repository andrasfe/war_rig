# CBPAUP0J

**File**: `jcl/CBPAUP0J.jcl`
**Type**: FileType.JCL
**Analyzed**: 2026-01-30 19:38:54.677425

## Purpose

This JCL job CBPAUP0J executes the IMS batch message processing (BMP) program CBPAUP0C via the DFSRRC00 region controller to delete expired authorizations. It configures IMS libraries, PSB/DBD libraries, and provides inline SYSIN parameters. Standard IMS batch DD statements are defined for inputs, outputs, and dumps.

**Business Context**: Manages authorizations in the CARDDEMO system by deleting expired entries from an IMS database using PSBPAUTB.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| SYSIN | IOType.PARAMETER | Inline control parameters '00,00001,00001,Y' passed to DFSRRC00 for IMS program execution (likely PCB/segment/position flags). |
| IMS.PSBLIB | IOType.OTHER | IMS PSB library containing PSBPAUTB definition. |
| IMS.DBDLIB | IOType.OTHER | IMS DBD library for database definitions accessed via PSBPAUTB. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard print output from DFSRRC00 and CBPAUP0C execution. |
| SYSUDUMP | IOType.REPORT | System dump output on program abend. |
| SYSOUT | IOType.REPORT | General job and IMS output listings. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS region controller to execute CBPAUP0C in BMP mode. |
| CBPAUP0C | CallType.DYNAMIC_CALL | Application program to delete expired authorizations using PSBPAUTB. |

## Paragraphs/Procedures

### CBPAUP0J
This paragraph corresponds to the JOB statement defining the batch job CBPAUP0J. Its primary purpose is to specify job-level parameters including jobname 'CBPAUP0J', account 'CARDDEMO', CLASS=A for scheduling, MSGCLASS=H for messages, MSGLEVEL=(1,1) for detail, REGION=0M for memory, and NOTIFY=&SYSUID for completion notification. It orchestrates the overall execution environment for deleting expired authorizations via IMS. It consumes no direct data inputs but sets the context for DD allocations in subordinate steps. It produces job logs, SMF records, and notifications as standard JCL outputs. No business logic or conditions are implemented at the JOB level; execution proceeds unconditionally to defined steps if submitted successfully. Error handling is implicit via JCL (e.g., job failure if no steps complete). It contains or 'calls' the STEP01 processing step. No validations or external program calls occur here. The job relies on STEP01 for all processing.

### STEP01
This is the sole processing step named STEP01 in the job, responsible for executing the IMS batch region controller DFSRRC00. Its primary role is to invoke CBPAUP0C in BMP mode with PSBPAUTB to perform deletion of expired authorizations. It consumes inputs from SYSIN parameters (00,00001,00001,Y), STEPLIB/DFSRESLB for load modules, PROCLIB/DFSSEL for procedures, and IMS/PSBLIB/DBDLIB for metadata. It produces outputs to multiple SYSOUT datasets including SYSOUX, SYSOUT, SYSABOUT, ABENDAID, SYSPRINT, SYSUDUMP, and IMSERR for logs, prints, and dumps. Business logic is delegated to CBPAUP0C; no decisions in JCL itself. Error handling includes DUMMY for IEFRDER/IMSLOGR, ABENDAID for abend aid, and SYSUDUMP for diagnostics on failures. It statically executes DFSRRC00 and dynamically loads CBPAUP0C via PARM. No subordinate paragraphs exist as this is JCL. File statuses are handled by IMS via dumps/prints. Control flow is linear: EXEC followed by DD definitions.

## Open Questions

- ? What is the exact meaning of SYSIN parameters '00,00001,00001,Y'?
  - Context: Parameters passed to DFSRRC00 but not explained in JCL comments.
- ? PSB name known but database definitions not in JCL.

## Sequence Diagram

```mermaid
sequenceDiagram
    STEP01->>DFSRRC00: performs
    STEP01->>IMS.SDFSRESL: performs
    STEP01->>XXXXXXXX.PROD.LOADLIB: performs
    STEP01->>IMS.SDFSRESL: performs
    STEP01->>IMS.PROCLIB: performs
    STEP01->>IMS.SDFSRESL: performs
    STEP01->>IMS.PSBLIB: performs
    STEP01->>IMS.DBDLIB: performs
```
