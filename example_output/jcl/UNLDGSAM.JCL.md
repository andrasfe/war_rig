# UNLDGSAM

**File**: `jcl/UNLDGSAM.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 02:34:29.464571

## Purpose

This JCL submits a batch job to execute the IMS utility program DFSRRC00 in DLI mode for unloading the IMS database DBUNLDGS defined by DBD DLIGSAMP. It provides access to GSAM input datasets PASFILOP (root) and PADFILOP (child), along with required IMS libraries and proclibs. Note: No UNLOADnn DD statements are defined for capturing sequential unload records, which is atypical; output may be print-only to SYSPRINT or not persisted to datasets.

**Business Context**: IMS database unloading for PAUTDB GSAM segments in CARDDEMO environment

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PARM | IOType.PARAMETER | Specifies DLI access method, database name DBUNLDGS, DBD name DLIGSAMP, and additional flags ending in N |
| PASFILOP | IOType.IMS_SEGMENT | Input GSAM dataset for root segments: AWS.M2.CARDDEMO.PAUTDB.ROOT.GSAM (DISP=OLD,KEEP,KEEP) |
| PADFILOP | IOType.IMS_SEGMENT | Input GSAM dataset for child segments: AWS.M2.CARDDEMO.PAUTDB.CHILD.GSAM (DISP=OLD,KEEP,KEEP) |
| DDPAUTP0 | IOType.IMS_SEGMENT | IMS PAUTHDB dataset (SHR) |
| DDPAUTX0 | IOType.IMS_SEGMENT | IMS PAUTHDBX dataset (SHR) |
| STEPLIB | IOType.OTHER | Load libraries including OEMA.IMS.IMSP.SDFSRESL, V151 variant, and AWS.M2.CARDDEMO.LOADLIB |
| IMS | IOType.OTHER | IMS PSBLIB and DBDLIB libraries |
| DFSVSAMP | IOType.OTHER | IMS proclib member DFSVSMDB |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard utility print output including any unload listings or diagnostics |
| SYSUDUMP | IOType.REPORT | System dump output for abends |
| IMSERR | IOType.REPORT | IMS error messages |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | IMS utility to unload database segments from DBUNLDGS (DLIGSAMP) in DLI mode |

## Open Questions

- ? Primary output dataset for unloaded records
  - Context: No UNLOADnn DD statements defined anywhere in JCL (lines 1-54); atypical for DFSRRC00 UNLOAD as sequential unload files are normally required per segment
