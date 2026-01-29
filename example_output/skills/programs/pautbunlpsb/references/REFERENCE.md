# PAUTBUNL - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** PAUTBUNL
- **File Name:** PAUTBUNL.PSB
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:55:19.699475

## Purpose

**Summary:** This PSB (Program Specification Block) defines the database access parameters for an IMS (Information Management System) application. It specifies the database (DBPAUTP0), the processing options (PROCOPT=GOTP), key length, and the segments (PAUTSUM0, PAUTDTL1) that the program can access. The PSB is designed for COBOL and is named PAUTBUNL.

**Business Context:** None
**Program Type:** UTILITY

## Inputs

### DBPAUTP0

- **Type:** IMS_SEGMENT
- **Description:** The IMS database accessed by the program, as defined by DBDNAME.

### PAUTSUM0

- **Type:** IMS_SEGMENT
- **Description:** The PAUTSUM0 segment within the DBPAUTP0 database.

### PAUTDTL1

- **Type:** IMS_SEGMENT
- **Description:** The PAUTDTL1 segment within the DBPAUTP0 database, a child segment of PAUTSUM0.

## Outputs

### DBPAUTP0

- **Type:** IMS_SEGMENT
- **Description:** The IMS database accessed by the program. Due to PROCOPT=GOTP, updates are possible.

### PAUTSUM0

- **Type:** IMS_SEGMENT
- **Description:** The PAUTSUM0 segment within the DBPAUTP0 database. Updates are possible due to PROCOPT=GOTP.

### PAUTDTL1

- **Type:** IMS_SEGMENT
- **Description:** The PAUTDTL1 segment within the DBPAUTP0 database, a child segment of PAUTSUM0. Updates are possible due to PROCOPT=GOTP.

## Data Flow

### Reads From

- **DBPAUTP0:** 
- **PAUTSUM0:** 
- **PAUTDTL1:** 

### Writes To

- **DBPAUTP0:** 
- **PAUTSUM0:** 
- **PAUTDTL1:** 
