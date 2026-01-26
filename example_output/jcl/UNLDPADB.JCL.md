# UNLDPADB

**File**: `jcl/UNLDPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 02:32:44.232598

## Purpose

This JCL job executes an IMS database unload using the DFSRRC00 utility to extract root segments from PAUDBUNL database and child segments from PAUTBUNL database into sequential files. STEP0 performs cleanup by deleting any existing output files. STEP01 runs the unload utility with DLI access method.

**Business Context**: Supports unloading of IMS PAUTHDB databases in the AWS.M2.CARDDEMO payment authorization demo environment for backup, migration, or analysis purposes.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTP0 | IOType.IMS_SEGMENT | IMS PAUTHDB primary database dataset containing PAUDBUNL root segments |
| DDPAUTX0 | IOType.IMS_SEGMENT | IMS PAUTHDBX secondary/index dataset associated with PAUTBUNL child segments |
| PARM | IOType.PARAMETER | Parameters for DFSRRC00 specifying DLI access, PAUDBUNL root, PAUTBUNL child, and unload options |
| DD1 | IOType.FILE_SEQUENTIAL | Existing ROOT.FILEO unload file targeted for deletion in cleanup |
| DD2 | IOType.FILE_SEQUENTIAL | Existing CHILD.FILEO unload file targeted for deletion in cleanup |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OUTFIL1 | IOType.FILE_SEQUENTIAL | Sequential unload file for PAUDBUNL root segments (LRECL=100, RECFM=FB) |
| OUTFIL2 | IOType.FILE_SEQUENTIAL | Sequential unload file for PAUTBUNL child segments (LRECL=206, RECFM=FB) |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Dummy execution to process DD deletes for cleanup of prior output files |
| DFSRRC00 | CallType.STATIC_CALL | IMS database unload utility to extract segments into sequential files |

## Business Rules

- **BR001**: Unload only PAUDBUNL root database and PAUTBUNL child database segments

## Paragraphs/Procedures

### STEP0
This is the initial cleanup step in the job, designed to remove any existing versions of the output unload files to prevent conflicts or overwrites during the subsequent unload process. It executes the IEFBR14 program, which performs no computation but allows JCL DD statements to be honored. Inputs consumed are DD1 (AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO) and DD2 (AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO), both accessed with DISP=(OLD,DELETE,DELETE) to delete if present. No outputs are produced; the step modifies the catalog/external storage by removing old files. There is no business logic or conditional decision-making; deletion is unconditional. Error handling is implicit in JCL: if files do not exist, the step completes normally without abend. It defines diagnostic DDs like SYSPRINT, SYSOUT, and SYSDUMP for any potential issues. This step is called first in job sequence and calls no other paragraphs or programs. Its role ensures a clean environment for STEP01.

### STEP01
This is the primary processing step that orchestrates the IMS database unload operation using the DFSRRC00 utility. Its primary purpose is to read segments from the specified IMS databases and write them as fixed-block sequential unload files for root and child segments. It consumes inputs from IMS libraries (STEPLIB, DFSRESLB, IMS PSBLIB/DBDLIB), database datasets DDPAUTP0 (PAUTHDB) and DDPAUTX0 (PAUTHDBX), and the PARM='DLI,PAUDBUNL,PAUTBUNL,,,,,,,,,,,N' specifying unload targets. Outputs produced are OUTFIL1 (root unload, LRECL=100) and OUTFIL2 (child unload, LRECL=206), both cataloged with space allocation. Business logic is encapsulated in the utility and PARM: DLI access for PAUDBUNL root and PAUTBUNL child with defaults and N option (likely suppressing secondary index unload). No explicit decisions in JCL; utility handles all validations and EOF. Error handling includes IMSERR, SYSUDUMP, SYSPRINT for diagnostics, with job abend on utility failure. Defines additional DDs like DFSVSAMP (VSMDB proclib), IMSLOGR (DUMMY). No subordinate paragraphs called; relies on prior STEP0 cleanup. This step controls the core business function of database unloading.

## Open Questions

- ? Exact interpretation of all comma-delimited fields in DFSRRC00 PARM beyond PAUTBUNL
  - Context: Multiple commas indicate default values not explicitly documented in JCL comments
