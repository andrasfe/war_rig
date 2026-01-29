# CBPAUP0J - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** CBPAUP0J
- **File Name:** CBPAUP0J.jcl
- **File Type:** JCL
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:55:41.693454

## Purpose

**Summary:** This JCL executes an IMS program (DFSRRC00) to delete expired authorizations, using the BMP region controller. It specifies the program CBPAUP0C and PSB PSBPAUTB for the IMS execution.

**Business Context:** This job likely supports security and compliance by removing outdated access privileges.
**Program Type:** BATCH

## Inputs

### IMS.SDFSRESL

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS RESLIB library containing IMS modules.

### XXXXXXXX.PROD.LOADLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** Application load library, likely containing CBPAUP0C.

### IMS.PROCLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS procedure library.

### IMS.PSBLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS PSB library containing PSBPAUTB.

### IMS.DBDLIB

- **Type:** FILE_SEQUENTIAL
- **Description:** IMS DBD library.

### SYSIN

- **Type:** FILE_SEQUENTIAL
- **Description:** Control input for the IMS program. Contains parameters '00,00001,00001,Y'.

## Outputs

### SYSOUX

- **Type:** REPORT
- **Description:** System output.

### SYSOUT

- **Type:** REPORT
- **Description:** System output.

### SYSABOUT

- **Type:** REPORT
- **Description:** System output for ABEND information.

### ABENDAID

- **Type:** REPORT
- **Description:** System output for ABEND aid.

### SYSPRINT

- **Type:** REPORT
- **Description:** System print output.

### SYSUDUMP

- **Type:** REPORT
- **Description:** System user dump output.

### IMSERR

- **Type:** REPORT
- **Description:** IMS error output.

## Paragraphs

### CBPAUP0J

[Citadel] Paragraph identified by static analysis

### STEP01

[Citadel] Paragraph identified by static analysis

## Data Flow
