# UNLDPADB

**File**: `jcl/UNLDPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 17:36:35.171039

## Purpose

This JCL job unloads the IMS database PAUTDB into two sequential flat files: root segments to PAUTDB.ROOT.FILEO and child segments to PAUTDB.CHILD.FILEO. STEP0 performs cleanup by deleting any existing output files using IEFBR14. STEP01 executes the IMS unload utility DFSRRC00 with parameters specifying DLI access, unload DBDs PAUDBUNL and PAUTBUNL.

**Business Context**: Supports AWS Mainframe Modernization (M2) CARDDEMO application by unloading IMS PAUTDB database for potential backup, migration, or demo purposes.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| STEPLIB | IOType.FILE_SEQUENTIAL | IMS SDFSRESL libraries and application LOADLIB for executing DFSRRC00 |
| DFSRESLB | IOType.FILE_SEQUENTIAL | IMS SDFSRESL library for utility execution |
| IMS | IOType.FILE_SEQUENTIAL | IMS PSBLIB and DBDLIB for PSB and DBD definitions |
| DDPAUTP0 | IOType.FILE_SEQUENTIAL | IMS PAUTHDB (primary authorization database library) |
| DDPAUTX0 | IOType.FILE_SEQUENTIAL | IMS PAUTHDBX (extended authorization database library) |
| DFSVSAMP | IOType.FILE_SEQUENTIAL | IMS proclib containing DFSVSMDB macro for database access |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OUTFIL1 | IOType.FILE_SEQUENTIAL | Unload file for PAUTDB root segments (LRECL=100, RECFM=FB) |
| OUTFIL2 | IOType.FILE_SEQUENTIAL | Unload file for PAUTDB child segments (LRECL=206, RECFM=FB) |
| SYSPRINT | IOType.REPORT | Standard print output for job logs |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | No-op execution to trigger deletion of old output files via DD DISP |
| DFSRRC00 | CallType.STATIC_CALL | IMS database unload utility to extract PAUTDB segments to flat files |

## Business Rules

- **BR001**: Delete existing output files before unload to prevent append or conflicts
- **BR002**: Allocate new output files with specific DCB and space parameters
- **BR003**: Use DLI access mode for unload without checkpointing (N flag)

## Paragraphs/Procedures

### STEP0
This step serves as a cleanup phase prior to the main unload operation, executing the no-op program IEFBR14 to trigger deletion of any pre-existing output datasets. It consumes the DD1 and DD2 datasets (old versions of ROOT.FILEO and CHILD.FILEO) specified with DISP=(OLD,DELETE,DELETE), ensuring no residual files interfere with new cataloging. No data is read or processed beyond the existence check for deletion; there are no working variables or computations. Outputs are limited to standard SYSOUT datasets like SYSPRINT, SYSOUT, and SYSDUMP for any job logs. No business logic or conditional decisions are implemented, as it is purely a housekeeping step without PERFORM or IF statements typical in COBOL. Error handling relies on standard JCL disposition failure, which would cause job ABEND if files are not deletable. It calls no subordinate steps, programs, or paragraphs, serving only to prepare the environment for STEP01. SYSDUMP is provided for diagnostics if deletion fails.

### STEP01
This is the primary processing step that executes the IMS database unload utility DFSRRC00 to extract segments from PAUTDB into flat files. It consumes IMS libraries (STEPLIB, DFSRESLB, IMS, DDPAUTP0, DDPAUTX0, DFSVSAMP) for program load, DBD/PSB definitions, and database access macros, plus the target IMS database PAUTDB accessed via DLI mode and unload DBDs PAUDBUNL/PAUTBUNL specified in PARM. Outputs are the newly cataloged sequential files OUTFIL1 (root segments) and OUTFIL2 (child segments) with specific DCB attributes, plus SYSOUT files for logs, dumps, and errors. Business logic is encapsulated in the utility's PARM, specifying dual unloads without checkpointing (N flag) for a complete one-pass extraction. No explicit validations or decisions are coded in JCL, deferring to DFSRRC00's internal handling of database status and segment hierarchies. Error handling uses DUMMY DDs (IMSLOGR, IEFRDER) to suppress unnecessary logs, with SYSUDUMP and IMSERR for capturing failures. It calls no other programs or steps, operating independently after STEP0 cleanup. The REGION=0M and TIME=1440 parameters accommodate potentially large database unloads.

## Open Questions

- ? Exact segment structure and field layouts in unloaded files
  - Context: JCL does not specify; depends on IMS DBD definitions in PAUTHDB
- ? Relationship between PAUTBUNL and child segments
  - Context: PARM lists PAUDBUNL then PAUTBUNL mapping to OUTFIL1/OUTFIL2
