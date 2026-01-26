# UNLDPADB

**File**: `jcl/UNLDPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 14:19:42.588457

## Purpose

This JCL defines a batch job to unload IMS database segments from PAUTDB (via PAUTHDB and PAUTHDBX) into two sequential flat files: ROOT.FILEO for root segments and CHILD.FILEO for child segments. STEP0 uses IEFBR14 to delete any prior versions of the output files. STEP01 executes the IMS utility DFSRRC00 in DLI mode to perform the unload operation.

**Business Context**: Supports AWS M2 CARDDEMO application by unloading IMS PAUTDB database for offline processing, archiving, or demo purposes

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTP0 | IOType.IMS_SEGMENT | IMS PAUTHDB database (primary input for PAUTDB unload) |
| DDPAUTX0 | IOType.IMS_SEGMENT | IMS PAUTHDBX database (secondary input for PAUTDB unload) |
| DFSVSAMP | IOType.OTHER | IMS proclib member DFSVSMDB providing database definitions |
| STEPLIB | IOType.OTHER | Load libraries containing IMS utilities and application load modules |
| DD1 | IOType.FILE_SEQUENTIAL | Prior version of ROOT.FILEO for deletion |
| DD2 | IOType.FILE_SEQUENTIAL | Prior version of CHILD.FILEO for deletion |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OUTFIL1 | IOType.FILE_SEQUENTIAL | Unloaded root segments from PAUTDB in fixed-block format (LRECL=100) |
| OUTFIL2 | IOType.FILE_SEQUENTIAL | Unloaded child segments from PAUTDB in fixed-block format (LRECL=206) |
| SYSPRINT | IOType.REPORT | Utility print output and messages |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Dummy execution to trigger DD disposition deletes for prior output files |
| DFSRRC00 | CallType.STATIC_CALL | IMS database unload utility to extract segments to sequential files |

## Business Rules

- **BR001**: Delete any existing output files from prior runs before unloading to prevent accumulation or overwrite issues
- **BR002**: Catalog new output files for permanent storage after successful unload

## Paragraphs/Procedures

### STEP0
This step serves as a cleanup phase to remove any existing output files from previous job executions, ensuring a fresh start for the unload process and preventing file conflicts or space issues. It consumes references to prior output files via DD1 (AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO) and DD2 (AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO), both specified with DISP=(OLD,DELETE,DELETE) to conditionally delete if present. No data is read or processed; the step produces file deletions as its primary output, with no new files created. There is no business logic or decision-making, as IEFBR14 performs no operations beyond establishing the step for DD processing. Error handling is implicit in JCL disposition: if files do not exist, the step succeeds without action; failures would cause job abend via system defaults. It executes PGM=IEFBR14 solely to enable the DD deletes, with no subordinate calls. SYSPRINT, SYSOUT, and SYSDUMP are directed to SYSOUT for any minimal logging. This step runs unconditionally as the first in the job.

### STEP01
This step is the core execution phase that performs the IMS database unload from PAUTDB into sequential files, serving as the primary business function of the job. It consumes IMS database inputs via DDPAUTP0 (PAUTHDB), DDPAUTX0 (PAUTHDBX), and DFSVSAMP (DFSVSMDB for definitions), along with required libraries in STEPLIB, DFSRESLB, and IMS for IMS utilities and metadata. Outputs are produced to OUTFIL1 (root segments, LRECL=100, cataloged) and OUTFIL2 (child segments, LRECL=206, cataloged), with SYSOUT datasets for logs, dumps, and errors. Business logic is driven by PARM='DLI,PAUDBUNL,PAUTBUNL,,,,,,,,,,,N', specifying DLI access method, unload DBD identifiers PAUDBUNL/PAUTBUNL, and no restart (N). No explicit conditions or validations are coded in JCL; all logic is within DFSRRC00 utility, which unloads all segments sequentially. Error handling includes SYSUDUMP, IMSERR, and IMSLOGR (DUMMY) for diagnostics, with job abend on utility failure. It executes PGM=DFSRRC00 with no subordinate paragraph calls, relying on IMS infrastructure DDs. Space allocations (400 tracks, 20/20 extents) ensure sufficient output capacity. This step depends on STEP0 completion implicitly via sequential job flow.

## Open Questions

- ? Exact segment names and structures in PAUTDB (root/child)
  - Context: JCL references files but no segment details visible
- ? Relationship between PAUTHDB/PAUTHDBX and PAUTDB/PAUDBUNL/PAUTBUNL
  - Context: Names similar but not identical; likely aliases or versions
