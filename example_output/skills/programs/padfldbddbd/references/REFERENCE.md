# PADFLDBD - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** PADFLDBD
- **File Name:** ims/PADFLDBD.DBD
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:32:56.466873

## Purpose

**Summary:** This DBD source file defines the IMS database PADFLDBD as a GSAM database with BSAM access method and no password protection. It specifies Dataset Group 1 (DSG001) with input dataset DD1=PADFILIP and output dataset DD2=PADFILOP, both using fixed-length records of 200 bytes (RECFM=F). The definition was prepared for DBDGEN processing on IMS version 15.1.

**Business Context:** None
**Program Type:** UTILITY

## Inputs

### PADFILIP

- **Type:** FILE_SEQUENTIAL
- **Description:** Input dataset (DD1) associated with Dataset Group 1 of the PADFLDBD GSAM database, fixed 200-byte records

## Outputs

### PADFILOP

- **Type:** FILE_SEQUENTIAL
- **Description:** Output dataset (DD2) associated with Dataset Group 1 of the PADFLDBD GSAM database, fixed 200-byte records

## Data Flow
