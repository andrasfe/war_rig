# UNLDPADB

**File**: `jcl/UNLDPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-26 15:10:41.724955

## Purpose

This JCL job unloads the IMS hierarchical database PAUTDB (root and child segments) into two sequential flat files using the IMS utility DFSRRC00. STEP0 first deletes any existing output files to ensure a clean start. STEP01 performs the actual unload operation under DLI access with specified PSB/DBD names.

**Business Context**: Supports AWS M2 CARDDEMO application by unloading PAUTDB IMS database, likely for batch processing, migration, or demo data extraction (citation: lines 33-56).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTP0 | IOType.IMS_SEGMENT | IMS PAUTHDB dataset (primary database access for unload) |
| DDPAUTX0 | IOType.IMS_SEGMENT | IMS PAUTHDBX dataset (extended database access for unload) |
| DFSVSAMP | IOType.OTHER | VSAM MDB definition from IMS PROCLIB(DFSVSMDB) for database control |
| DD1 | IOType.FILE_SEQUENTIAL | Existing ROOT.FILEO file targeted for deletion if present |
| DD2 | IOType.FILE_SEQUENTIAL | Existing CHILD.FILEO file targeted for deletion if present |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OUTFIL1 | IOType.FILE_SEQUENTIAL | Unloaded root segments from PAUTDB (LRECL=100, RECFM=FB) |
| OUTFIL2 | IOType.FILE_SEQUENTIAL | Unloaded child segments from PAUTDB (LRECL=206, RECFM=FB) |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| IEFBR14 | CallType.STATIC_CALL | Execute no-op to trigger deletion of prior output files via DISP=OLD,DELETE |
| DFSRRC00 | CallType.STATIC_CALL | IMS utility to unload PAUTDB root and child segments to sequential files |

## Paragraphs/Procedures

### STEP0
This step serves as a cleanup procedure to remove any existing output files from previous runs, ensuring no conflicts or overwrites without explicit deletion. It executes the IBM utility IEFBR14, which performs no computation but activates the DD statements' DISP parameters. Inputs consumed are the potential existence of prior output files referenced by DD1 (AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO) and DD2 (AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO), both with DISP=(OLD,DELETE,DELETE) to delete if present or pass if not. Outputs produced are the deletions themselves, with no data files written; SYSPRINT, SYSOUT, and SYSDUMP are directed to SYSOUT for any diagnostic traces. No business logic or conditional decisions are implemented, as it is a simple housekeeping step. Error handling relies on standard JCL disposition failure (e.g., uncataloged files would ABEND STEP0). It calls no other steps or programs, serving only as a prerequisite for STEP01. SYSDUMP is provided for debugging any deletion issues.

### STEP01
This is the primary processing step that executes the IMS database unload utility DFSRRC00 to extract segments from PAUTDB into flat sequential files. It consumes inputs from multiple library DD statements including STEPLIB (IMS SDFSRESL modules and application LOADLIB), DFSRESLB (IMS RESLIB), IMS (PSBLIB and DBDLIB for PSB/DBD definitions), DDPAUTP0/ DDPAUTX0 (PAUTHDB/PAUTHDBX datasets for database access), and DFSVSAMP (VSAM MDB from PROCLIB). The PARM specifies DLI access, PSB=PAUDBUNL, DBD=PAUTBUNL, and other defaults for unload mode with no restart. Outputs produced are OUTFIL1 (root segments, LRECL=100, SPACE allocated) and OUTFIL2 (child segments, LRECL=206, SPACE allocated), plus SYSOUTs for logs (SYSPRINT, SYSUDUMP, IMSERR). Business logic is encapsulated in the DFSRRC00 utility, which traverses the IMS database hierarchy and unloads segments sequentially without specified conditions. Error handling uses DUMMY DDs (IMSLOGR, IEFRDER) to suppress unnecessary logs and SYSOUTs for diagnostics; failures would ABEND the step per JCL standards. No subordinate paragraphs or programs are called; it is self-contained. The step ensures cataloged outputs with RLSE for space efficiency.

## Open Questions

- ? Exact segment names and layouts in PAUTDB root/child?
  - Context: JCL references databases generically; no segment details visible
- ? Precise mapping of PARM fields (e.g., PAUDBUNL as PSB vs DBD)?
  - Context: IMS DFSRRC00 PARM format inferred but not explicitly documented in JCL
