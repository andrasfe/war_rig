# PASFLDBD - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** PASFLDBD
- **File Name:** ims/PASFLDBD.DBD
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:34:04.190920

## Purpose

**Summary:** This DBDGEN source file defines the IMS database named PASFLDBD using GSAM and BSAM access methods without password protection. It specifies a single dataset group DSG001 with input dataset DD1=PASFILIP and output dataset DD2=PASFILOP, both using fixed-length records (RECFM=F) of 100 bytes. The definition was generated on 04/21/2023 for IMS version 15.1.

**Business Context:** None
**Program Type:** UTILITY

## Inputs

### PASFILIP

- **Type:** FILE_SEQUENTIAL
- **Description:** Input dataset (DD1) for the PASFLDBD GSAM database, referenced in dataset group DSG001

## Outputs

### PASFILOP

- **Type:** FILE_SEQUENTIAL
- **Description:** Output dataset (DD2) for the PASFLDBD GSAM database, referenced in dataset group DSG001

## Data Flow
