# UNLDPADB

**File**: `jcl/UNLDPADB.JCL`
**Type**: FileType.JCL
**Analyzed**: 2026-01-25 18:30:35.504058

## Purpose

This JCL deletes existing output files in STEP0 using IEFBR14. In STEP01, it executes the IMS utility DFSRRC00 to unload root and child segments from the PAUTDB IMS database into sequential files ROOT.FILEO and CHILD.FILEO. The PARM specifies DLI access method and PSB names PAUDBUNL and PAUTBUNL for the unloads.

**Business Context**: Unloading IMS PAUTDB database for AWS M2 Card Demo application

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PARM | IOType.PARAMETER | DFSRRC00 parameters specifying DLI, PAUDBUNL, PAUTBUNL unloads, and other options |
| DDPAUTP0 | IOType.IMS_SEGMENT | IMS database dataset PAUTHDB accessed SHR for unloading |
| DDPAUTX0 | IOType.IMS_SEGMENT | IMS database dataset PAUTHDBX accessed SHR for unloading |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OUTFIL1 | IOType.FILE_SEQUENTIAL | Sequential file containing unloaded PAUTDB root segments |
| OUTFIL2 | IOType.FILE_SEQUENTIAL | Sequential file containing unloaded PAUTDB child segments |

## Business Rules

- **BR001**: Deletes any existing versions of output unload files prior to new unload
- **BR002**: Performs unload of root segments using PAUDBUNL specification
- **BR003**: Performs unload of child segments using PAUTBUNL specification

## Paragraphs/Procedures

### STEP0
Deletes prior output files using IEFBR14 and DISP=OLD,DELETE

### STEP01
Executes DFSRRC00 IMS utility for database unload to sequential files

## Open Questions

- ? Which DD (DDPAUTP0 or DDPAUTX0) corresponds to root vs child segments
  - Context: PARM specifies PAUDBUNL and PAUTBUNL but DD names do not explicitly map
- ? Role of STEPLIB and other IMS library DDs
  - Context: Multiple IMS libraries concatenated but specific modules not identifiable from JCL alone
