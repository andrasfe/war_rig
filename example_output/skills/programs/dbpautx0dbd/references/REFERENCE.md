# DBPAUTX0 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** DBPAUTX0
- **File Name:** ims/DBPAUTX0.dbd
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:42:53.031631

## Purpose

**Summary:** This file contains the source for the IMS Database Definition (DBD) of DBPAUTX0, an indexed VSAM-protected database. It defines one dataset group DSG001 with dataset DDPAUTX0 of size 4096. It specifies a single root segment PAUTINDX (6 bytes) with unique sequential packed key INDXSEQ and a logical child to PAUTSUM0 in DBPAUTP0 via index ACCNTID.

**Business Context:** None
**Program Type:** UTILITY

## Inputs

### DDPAUTX0

- **Type:** FILE_VSAM
- **Description:** Primary dataset referenced in dataset group DSG001 with size 4096

## Business Rules

### BR001

**Description:** Unique sequential key INDXSEQ ensures each PAUTINDX segment is uniquely identified and maintained in sequence order

**Logic:** Defined via FIELD NAME=(INDXSEQ,SEQ,U)

### BR002

**Description:** Segment PAUTINDX has an expected frequency of 100000 occurrences for performance and space planning

**Logic:** Specified by FREQ=100000

### BR003

**Description:** Logical child relationship to PAUTSUM0 segment in database DBPAUTP0 accessed via index ACCNTID

**Logic:** Defined via LCHILD NAME=(PAUTSUM0,DBPAUTP0), INDEX=ACCNTID

## Paragraphs

### SEGM PAUTINDX

This section defines the primary (and only) segment structure for the DBPAUTX0 database. It declares PAUTINDX as a root segment (PARENT=0) with a total length of 6 bytes and expected frequency of 100000. It consumes no runtime inputs but structurally references the dataset from DSG001 for storage. It defines the key field INDXSEQ as a 6-byte packed unique sequential field starting at position 1, which serves as the primary ordering and uniqueness mechanism. It establishes a logical child pointer to PAUTSUM0 in another database DBPAUTP0 using the ACCNTID index for hierarchical navigation. No explicit business decisions or validations are coded here as this is a static definition. Error handling is not applicable in the DBD source itself. It culminates in DBDGEN to generate the database description and FINISH to end the definition. This segment definition enables DL/I application programs to insert, retrieve, and traverse PAUTINDX records and their logical children.

## Data Flow
