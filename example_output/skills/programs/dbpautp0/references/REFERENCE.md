# DBPAUTP0 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** DBPAUTP0
- **File Name:** jcl/DBPAUTP0.jcl
- **File Type:** JCL
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:55:57.668269

## Purpose

**Summary:** This JCL job unloads the DBD DBPAUTP0 from an IMS database. It first deletes the output dataset if it exists, then executes the DFSRRC00 program with the ULU parameter to perform the unload. It allocates necessary datasets for IMS processing, including RESLIB, PSBLIB, DBDLIB, and RECON datasets.

**Business Context:** None
**Program Type:** BATCH

## Inputs

### OEM.IMS.IMSP.PSBLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS PSB Library

### OEM.IMS.IMSP.DBDLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS DBD Library

### OEM.IMS.IMSP.PAUTHDB

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS PAUTHDB

### OEM.IMS.IMSP.PAUTHDBX

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS PAUTHDBX

### OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB)

- **Type:** FILE_SEQUENTIAL
- **Description:** DFSVSMDB proc

### OEM.IMS.IMSP.RECON1

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS RECON1 dataset

### OEM.IMS.IMSP.RECON2

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS RECON2 dataset

### OEM.IMS.IMSP.RECON3

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS RECON3 dataset

### OEMA.IMS.IMSP.SDFSRESL

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS RESLIB

### AWS.M2.CARDDEMO.LOADLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** Load library

## Outputs

### AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0

- **Type:** FILE_VSAM
- **Description:** Output dataset containing the unloaded DBD DBPAUTP0 data.

### SYSUDUMP

- **Type:** REPORT
- **Description:** System dump output.

### SYSPRINT

- **Type:** REPORT
- **Description:** System print output.

## Paragraphs

### STEPDEL

This step deletes the output dataset AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0 before the unload process. It executes the IEFBR14 program, a utility used for dataset allocation and deletion. The SYSUT1 DD statement defines the dataset to be deleted, specifying a disposition of MOD and DELETE. This ensures that if the dataset already exists, it will be deleted. The SYSPRINT DD statement directs the output of the deletion process to the system output. This step prepares the environment for the subsequent unload step by ensuring a clean slate for the output data.

### UNLOAD

**(Dead Code)**

This step performs the actual unload of the DBD DBPAUTP0. It executes the DFSRRC00 program, which is a generic IMS resource control program. The PARM parameter specifies ULU (Unload Utility), DFSURGU0 (Unload control statements), and DBPAUTP0 (the database to unload). The STEPLIB and DFSRESLB DD statements define the IMS RESLIB, providing access to the necessary IMS modules. The IMS DD statement defines the PSBLIB and DBDLIB. The DFSURGU1 DD statement defines the output dataset where the unloaded data will be stored. The DDPAUTP0 and DDPAUTX0 DD statements define the input database datasets. The DFSVSAMP DD statement points to the DFSVSMDB proc, which contains buffer pool parameters. The DFSCTL DD statement contains control parameters, in this case SBPARM ACTIV=COND. The SYSUDUMP DD statement defines the system dump dataset. The RECON* DD statements define the RECON datasets required for IMS database access. DFSWRK01 and DFSSRT01 are dummy datasets.

## Data Flow

## Dead Code

- **UNLOAD** (paragraph): Paragraph 'UNLOAD' is never PERFORMed or referenced by any other paragraph or program
