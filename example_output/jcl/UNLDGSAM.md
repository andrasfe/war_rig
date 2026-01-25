# UNLDGSAM

**File**: `jcl/UNLDGSAM.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-25 18:30:35.053671

## Purpose

This JCL defines a batch job that executes the IMS utility program DFSRRC00 to process GSAM datasets for the PAUTDB database. It accesses primary (ROOT.GSAM) and child/overflow (CHILD.GSAM) files using DLI method for database DBUNLDGS and DBD DLIGSAMP. Outputs are directed to SYSOUT datasets for printing, dumps, and errors.

**Business Context**: Supports AWS M2 CardDemo application by unloading or processing IMS PAUTDB GSAM database (inferred from dataset names AWS.M2.CARDDEMO.PAUTDB.*.GSAM).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFILOP | IOType.FILE_VSAM | GSAM dataset AWS.M2.CARDDEMO.PAUTDB.ROOT.GSAM opened with DISP=OLD for input processing |
| PADFILOP | IOType.FILE_VSAM | GSAM dataset AWS.M2.CARDDEMO.PAUTDB.CHILD.GSAM opened with DISP=OLD for input processing |
| DDPAUTP0 | IOType.FILE_SEQUENTIAL | IMS dataset OEM.IMS.IMSP.PAUTHDB accessed SHR, likely containing DBD definitions for PAUTDB |
| DDPAUTX0 | IOType.FILE_SEQUENTIAL | IMS dataset OEM.IMS.IMSP.PAUTHDBX accessed SHR, likely additional DBD or index definitions |
| IMS | IOType.OTHER | IMS parameter library (PSBLIB) and DBD library (DBDLIB) for utility execution |
| DFSVSAMP | IOType.OTHER | IMS proclib member DFSVSMDB for database definitions in batch DLI mode |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| SYSPRINT | IOType.REPORT | Standard print output from IMS utility DFSRRC00 |
| SYSUDUMP | IOType.OTHER | System dump output for abends |
| IMSERR | IOType.REPORT | IMS-specific error messages |

## Business Rules

- **BR001**: Execute IMS utility in DLI mode for database DBUNLDGS using DBD DLIGSAMP
- **BR002**: Not applicable: JCL defines resources but no conditional business logic

## Paragraphs/Procedures

### STEP01
Main (and only) job step executing IMS utility DFSRRC00 with specified libraries and datasets

## Open Questions

- ? Exact function of DFSRRC00 with PARM='DLI,DBUNLDGS,DLIGSAMP,,,,,,,,,,,N' (unload, reconcile, or other?)
  - Context: Job name suggests GSAM unload, but no standard UNLOAD DD dataset defined and DFSRRC00 typically for reconcile utilities
- ? Specific fields or segments read/written from GSAM datasets
  - Context: JCL does not define record layouts; no copybooks
