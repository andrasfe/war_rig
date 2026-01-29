# UNLDGSAM - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** UNLDGSAM
- **File Name:** jcl/UNLDGSAM.JCL
- **File Type:** JCL
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:55:41.670823

## Purpose

**Summary:** This JCL job executes the IMS program DFSRRC00 to unload a GSAM database. It specifies the program parameters, required libraries, and input/output datasets for the IMS database unload process.

**Business Context:** None
**Program Type:** BATCH

## Inputs

### AWS.M2.CARDDEMO.PAUTDB.ROOT.GSAM

- **Type:** FILE_SEQUENTIAL
- **Description:** Input GSAM database root segment to be unloaded.

### AWS.M2.CARDDEMO.PAUTDB.CHILD.GSAM

- **Type:** FILE_SEQUENTIAL
- **Description:** Input GSAM database child segment to be unloaded.

### OEM.IMS.IMSP.PAUTHDB

- **Type:** FILE_SEQUENTIAL
- **Description:** Input IMS PAUTHDB dataset.

### OEM.IMS.IMSP.PAUTHDBX

- **Type:** FILE_SEQUENTIAL
- **Description:** Input IMS PAUTHDBX dataset.

### OEMA.IMS.IMSP.SDFSRESL

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS RESLIB

### OEMA.IMS.IMSP.SDFSRESL.V151

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS RESLIB version 151

### AWS.M2.CARDDEMO.LOADLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** Load library for the application

### OEM.IMS.IMSP.PSBLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS PSBLIB

### OEM.IMS.IMSP.DBDLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS DBDLIB

### OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB)

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS VSAM definition

## Outputs

### SYSPRINT

- **Type:** REPORT
- **Description:** System print output for the job.

### SYSUDUMP

- **Type:** REPORT
- **Description:** System dump output for the job.

### IMSERR

- **Type:** REPORT
- **Description:** IMS error output for the job.

## Paragraphs

### STEP01

This step executes the IMS program DFSRRC00, which is the DL/I batch utility used to unload a GSAM database. The PARM parameter specifies the execution parameters for the IMS program, including the database unload function (DBUNLDGS) and the DLIGSAMP control region. The STEPLIB DD statements define the libraries required to execute the IMS program, including the IMS RESLIB and the application load library. The IMS DD statement defines the PSBLIB and DBDLIB datasets required for the IMS execution. The PASFILOP and PADFILOP DD statements define the input GSAM database datasets to be unloaded. The DFSVSAMP DD statement specifies the VSAM buffer pool parameters. The SYSPRINT, SYSUDUMP, and IMSERR DD statements define the output datasets for system messages, dumps, and IMS error messages, respectively. The IEFRDER and IMSLOGR DD statements are dummy datasets.

## Data Flow

### Reads From

- **AWS.M2.CARDDEMO.PAUTDB.ROOT.GSAM:** 
- **AWS.M2.CARDDEMO.PAUTDB.CHILD.GSAM:** 
- **OEM.IMS.IMSP.PAUTHDB:** 
- **OEM.IMS.IMSP.PAUTHDBX:** 
