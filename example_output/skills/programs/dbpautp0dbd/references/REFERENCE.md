# DBPAUTP0 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** DBPAUTP0
- **File Name:** ims/DBPAUTP0.dbd
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:40:55.366051

## Purpose

**Summary:** This DBD source file defines the IMS HIDAM database DBPAUTP0 using VSAM access for pending authorization data. It specifies dataset DDPAUTP0, root segment PAUTSUM0 (100 bytes) with unique sequential key ACCNTID, and child segment PAUTDTL1 (200 bytes) with unique sequential key PAUT9CTS parented by PAUTSUM0. The file is processed by the DBDGEN utility to generate the DBD control block.

**Business Context:** Stores summary and detail records for pending account authorizations, supporting hierarchical access via root account ID.
**Program Type:** UTILITY

## Outputs

### DDPAUTP0

- **Type:** FILE_VSAM
- **Description:** VSAM dataset hosting the HIDAM database segments

### PAUTSUM0

- **Type:** IMS_SEGMENT
- **Description:** Root segment containing pending authorization summary data, 100 bytes

### PAUTDTL1

- **Type:** IMS_SEGMENT
- **Description:** Child segment containing pending authorization detail data, 200 bytes

## Business Rules

### BR001

**Description:** Root segment PAUTSUM0 is uniquely sequenced by ACCNTID field

**Logic:** Defined as sequential unique (SEQ,U) key starting at byte 1

**Conditions:**
- `SEQ,U key enforcement`

### BR002

**Description:** Child segment PAUTDTL1 is parented exclusively by root segment PAUTSUM0

**Logic:** PARENT=((PAUTSUM0,)) specifies hierarchical relationship

**Conditions:**
- `Parent must exist for child access`

### BR003

**Description:** Child segment PAUTDTL1 is uniquely sequenced by PAUT9CTS field

**Logic:** Defined as sequential unique (SEQ,U) key starting at byte 1

**Conditions:**
- `SEQ,U key enforcement`

## Data Flow
