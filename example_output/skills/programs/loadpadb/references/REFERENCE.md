# LOADPADB - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** LOADPADB
- **File Name:** jcl/LOADPADB.JCL
- **File Type:** JCL
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:55:30.595680

## Purpose

**Summary:** This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and input files required for the database load process.

**Business Context:** Database maintenance and loading.
**Program Type:** BATCH

## Inputs

### OEMA.IMS.IMSP.SDFSRESL

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS RESLIB library.

### AWS.M2.CARDDEMO.LOADLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** Application load library.

### OEM.IMS.IMSP.PSBLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS PSB library.

### OEM.IMS.IMSP.DBDLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS DBD library.

### AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO

- **Type:** FILE_SEQUENTIAL
- **Description:** Input file for the PAUTDB root segment.

### AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO

- **Type:** FILE_SEQUENTIAL
- **Description:** Input file for the PAUTDB child segment.

### OEMPP.IMS.V15R01MB.PROCLIB(DFSVSMDB)

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS VSAM definition.

## Outputs

### SYSPRINT

- **Type:** REPORT
- **Description:** System print output.

### SYSUDUMP

- **Type:** REPORT
- **Description:** System dump output.

### IMSERR

- **Type:** REPORT
- **Description:** IMS error output.

## Paragraphs

### STEP01

This step executes the IMS program DFSRRC00 in a BMP region to load the PAUTDB database. It defines the necessary libraries (STEPLIB, DFSRESLB, IMS) and input files (INFILE1, INFILE2) required for the database load process. The PARM parameter specifies the execution environment as BMP, the transaction PAUDBLOD, and the PSB PSBPAUTB. The STEPLIB DD statements define the libraries containing the IMS modules and the application load library. The DFSRESLB DD statement defines the IMS RESLIB library. The IMS DD statements define the PSB and DBD libraries. INFILE1 and INFILE2 DD statements define the input files for the root and child segments of the PAUTDB database, respectively. DFSVSAMP defines the VSAM parameters. The SYSPRINT, SYSUDUMP, and IMSERR DD statements define the output datasets for system messages, dumps, and IMS error messages, respectively.

## Data Flow

### Reads From

- **AWS.M2.CARDDEMO.PAUTDB.ROOT.FILEO:** 
- **AWS.M2.CARDDEMO.PAUTDB.CHILD.FILEO:** 
