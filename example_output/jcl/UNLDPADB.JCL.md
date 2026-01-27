# UNLDPADB

**File**: `jcl/UNLDPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-27 02:39:44.120788

## Purpose

This JCL job unloads the IMS database PAUTDB into two sequential flat files, separating root and child segments. STEP0 deletes any prior versions of the output files. STEP01 executes the IMS utility DFSRRC00 using DLI access method with specified PSB (PAUDBUNL) and DBD (PAUTBUNL) to perform the unload.

**Business Context**: Prepares sequential unload files from IMS PAUTDB database for AWS M2 Card Demo application, likely for data migration, backup, or offline processing.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTP0 | IOType.FILE_SEQUENTIAL | IMS PSB library containing PAUTHDB for PAUTDB unload PSB PAUDBUNL |
| DDPAUTX0 | IOType.FILE_SEQUENTIAL | IMS DBDX library containing PAUTHDBX for PAUTDB unload DBD PAUTBUNL |
| DFSVSAMP | IOType.FILE_SEQUENTIAL | IMS VSAM macro library member DFSVSMDB for utility support |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OUTFIL1 | IOType.FILE_SEQUENTIAL | Sequential file containing unloaded root segments from PAUTDB (LRECL=100, RECFM=FB) |
| OUTFIL2 | IOType.FILE_SEQUENTIAL | Sequential file containing unloaded child segments from PAUTDB (LRECL=206, RECFM=FB) |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Perform housekeeping deletion of prior output files |
| DFSRRC00 | CallType.STATIC_CALL | Execute IMS database unload utility for PAUTDB |

## Business Rules

- **BR001**: Prior output files must be deleted before new unload to ensure clean output
- **BR002**: Unload uses DLI access method with specific PSB and DBD names

## Paragraphs/Procedures

### STEP0
This step serves as a housekeeping procedure to delete any existing versions of the output unload files before the main unload operation begins. It executes the dummy program IEFBR14, which performs no computation but processes all DD statements. It consumes no input data files but references DD1 and DD2 pointing to the prior DSNs AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO and CHILD.FILEO with DISP=(OLD,DELETE,DELETE), ensuring they are deleted if present. No outputs are produced beyond potential SYSOUT messages to SYSPRINT, SYSOUT, and SYSDUMP. There is no business logic or conditional decision-making; it unconditionally attempts deletion. Error handling is implicit via JCL: if files do not exist, the step abends with JCL error, preventing STEP01 from running. It defines dummy DDs for standard output. This step is the entry point of the job and passes control to STEP01 upon success. No other paragraphs or programs are called.

### STEP01
This is the primary processing step that performs the actual unload of the IMS PAUTDB database into sequential files. It executes IMS utility DFSRRC00 with PARM='DLI,PAUDBUNL,PAUTBUNL,,,,,,,,,,,N' specifying DLI access, unload PSB PAUDBUNL, DBD PAUTBUNL, and no statistics output. It consumes IMS control files from STEPLIB (SDFSRESL libraries and loadlib), DFSRESLB, IMS (PSBLIB/DBDLIB), DDPAUTP0 (PAUTHDB PSB), DDPAUTX0 (PAUTHDBX DBDX), and DFSVSAMP (DFSVSMDB). The utility reads all root and child segments from PAUTDB IMS database. It produces two outputs: OUTFIL1 for root segments (new cataloged DSN with specified DCB/SPACE) and OUTFIL2 for child segments. Business logic is encapsulated in the utility: separates root/child based on DBD definition, formats as FB sequential. Validation and error handling are handled by IMS utility, with dumps to SYSUDUMP, IMSERR, IMSLOGR (DUMMY), and SYSPRINT. Dummy DDs like IEFRDER prevent unnecessary I/O. Called unconditionally after STEP0, it completes the job upon success. No subordinate paragraphs; relies on utility internals.

## Open Questions

- ? Precise role of DFSRRC00 in this context
  - Context: Non-standard program name for IMS unload (typically DFSURGL0); confirmed as unload by job name, outputs, and PARM format
- ? Specific segment/field layouts in PAUTDB
  - Context: Not visible in JCL; inferred root/child separation only
