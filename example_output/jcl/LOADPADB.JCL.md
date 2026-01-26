# LOADPADB

**File**: `jcl/LOADPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 14:19:44.076289

## Purpose

This JCL job executes the IMS utility DFSRRC00 in BMP mode to load the PAUDB database (DBD=PAUDBLOD) using PSB=PSBPAUTB. It reads root and child segment data from sequential unload files INFILE1 and INFILE2. Standard IMS libraries and control datasets are referenced, with output directed to SYSOUT datasets.

**Business Context**: Supports data loading for the AWS.M2.CARDDEMO application by reconstructing the IMS hierarchical database PAUTDB from unload files.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | IOType.FILE_SEQUENTIAL | Sequential unload file containing root segments for PAUTDB database |
| INFILE2 | IOType.FILE_SEQUENTIAL | Sequential unload file containing child segments for PAUTDB database |
| PARM | IOType.PARAMETER | Parameters controlling IMS utility: BMP mode, DBD=PAUDBLOD, PSB=PSBPAUTB |
| IMS | IOType.OTHER | PSBLIB and DBDLIB datasets providing IMS program specification and database definitions |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUDBLOD | IOType.IMS_SEGMENT | IMS database (DBD=PAUDBLOD) loaded with root and child segments from input files |
| SYSPRINT | IOType.REPORT | Standard print output from IMS utility execution |
| SYSUDUMP | IOType.REPORT | System dump output for diagnostics |
| IMSERR | IOType.REPORT | IMS error log output |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| DFSRRC00 | CallType.STATIC_CALL | Invoke IMS utility to load/reorganize PAUDBLOD database from sequential input files |

## Paragraphs/Procedures

### STEP01
This is the only job step in the LOADPADB JCL, serving as the primary execution point for the IMS database load utility. Its main role is to invoke DFSRRC00 with PARM='BMP,PAUDBLOD,PSBPAUTB' to process the PAUDBLOD database in batch message processing mode using the specified PSB (line 27). It consumes sequential input files INFILE1 for root segments (line 36) and INFILE2 for child segments (line 38), which contain unloaded database records from the AWS.M2.CARDDEMO.PAUTDB datasets. Supporting inputs include STEPLIB datasets for IMS libraries (lines 28-30), DFSRESLB (line 31), IMS PSBLIB/DBDLIB (lines 33-34), and DFSVSAMP for VSAM model database definitions (lines 44-45). Outputs are produced to the target IMS database PAUDBLOD implicitly via the utility, along with diagnostic reports to SYSPRINT, SYSUDUMP, and IMSERR on SYSOUT=* (lines 48-50). Dummy DDs like IMSLOGR and IEFRDER are provided for utility requirements (lines 46-47). No explicit business decisions or validations are implemented in the JCL itself; all logic is delegated to the IMS utility DFSRRC00. Error handling relies on the utility's return codes and standard JCL abend processing, with no COND or IF/THEN statements present. Commented DDs for PAUTHDB (lines 40-41) indicate potential alternative configurations. Overall, this step orchestrates the database load to support application data initialization.

## Open Questions

- ? Precise function of DFSRRC00 with PARM='BMP,PAUDBLOD,PSBPAUTB' (load, unload, or reorg)?
  - Context: Inferred as database load from job name LOADPADB, input unload files, and context, but exact utility mode undocumented in source.
