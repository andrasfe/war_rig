# UNLDPADB - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** UNLDPADB
- **File Name:** jcl/UNLDPADB.JCL
- **File Type:** JCL
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:44:58.994672

## Purpose

**Summary:** This JCL job first deletes any existing sequential output files from prior runs using IEFBR14, then executes the IMS Database Unload utility DFSRRC00 to unload the PAUTHDB and PAUTHDBX IMS databases into sequential files ROOT.FILEO and CHILD.FILEO. The unload targets root and child segments based on output file naming and PARM specifications (PAUDBUNL, PAUTBUNL). It supports the CARDDEMO application by creating flat files from the hierarchical IMS database.

**Business Context:** Unloading IMS PAUTDB/PAUTHDB database for backup, migration, or processing in the AWS.M2.CARDDEMO application
**Program Type:** BATCH

## Inputs

### DDPAUTP0

- **Type:** IMS_SEGMENT
- **Description:** IMS PAUTHDB database (primary/root segments for unload)

### DDPAUTX0

- **Type:** IMS_SEGMENT
- **Description:** IMS PAUTHDBX database (secondary/child segments for unload)

### DD1

- **Type:** FILE_SEQUENTIAL
- **Description:** Prior ROOT.FILEO for deletion if exists

### DD2

- **Type:** FILE_SEQUENTIAL
- **Description:** Prior CHILD.FILEO for deletion if exists

## Outputs

### OUTFIL1

- **Type:** FILE_SEQUENTIAL
- **Description:** Unloaded root segments from PAUTHDB in fixed-block format (LRECL=100)

### OUTFIL2

- **Type:** FILE_SEQUENTIAL
- **Description:** Unloaded child segments from PAUTHDBX in fixed-block format (LRECL=206)

## Paragraphs

### STEP0

This step serves as a cleanup procedure to ensure no residual output files from previous job executions interfere with the current run. It executes the no-op utility IEFBR14, which triggers file deletions based on DISP=(OLD,DELETE,DELETE) parameters for DD1 and DD2. Inputs are references to potential existing files AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO and AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO; if present, they are deleted, otherwise the step succeeds without action. No data is read or processed beyond existence checks implicit in DISP processing. Outputs are the deletion of those files to provide a clean slate. There are no business decisions, conditions, or validations explicitly coded; it relies on standard JCL file disposition behavior. No subordinate calls or error branches are present. Logging is directed to SYSPRINT, SYSOUT, and SYSDUMP on SYSOUT* for any issues. This step prevents catalog conflicts or overwrites in the subsequent unload step.

**Calls:** IEFBR14

### STEP01

**(Dead Code)**

This is the primary processing step that performs the IMS database unload using the DFSRRC00 utility. It consumes IMS database datasets from DDPAUTP0 (PAUTHDB) and DDPAUTX0 (PAUTHDBX), along with control libraries from STEPLIB, DFSRESLB, IMS (PSBLIB/DBDLIB), and DFSVSAMP (DBD definition). The PARM specifies DLI access, DBD names PAUDBUNL/PAUTBUNL, and other unload options ending with 'N'. Outputs are written to newly created and cataloged sequential files OUTFIL1 (root segments) and OUTFIL2 (child segments) with defined DCB (RECFM=FB, LRECLs 100/206) and space allocations. Business logic is encapsulated in the utility: hierarchical IMS segments are unloaded to flat sequential format without specified field-level transforms. No explicit conditions or decisions in JCL; the utility handles EOF, errors internally. Error handling relies on dummy DDs (IMSLOGR, IEFRDER) to suppress logs and SYSUDUMP/IMSERR for diagnostics on SYSOUT*. It calls no other programs or paragraphs. Upon success, the output files contain all database records ready for reload or further processing.

**Calls:** DFSRRC00

## Data Flow

### Reads From

- **DDPAUTP0 (PAUTHDB):** 
- **DDPAUTX0 (PAUTHDBX):** 

### Writes To

- **OUTFIL1 (ROOT.FILEO):** 
- **OUTFIL2 (CHILD.FILEO):** 

## Error Handling

- **File not found or I/O errors in STEP0 deletes:** JCL DISP processing continues successfully (OLD,DELETE tolerates non-existence)
- **IMS unload errors or abends:** Dumps to SYSUDUMP and IMSERR on SYSOUT*; dummy logs prevent interference

## Dead Code

- **STEP01** (paragraph): Paragraph 'STEP01' is never PERFORMed or referenced by any other paragraph or program
