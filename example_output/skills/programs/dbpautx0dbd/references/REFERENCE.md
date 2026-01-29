# DBPAUTX0 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** DBPAUTX0
- **File Name:** DBPAUTX0.dbd
- **File Type:** OTHER
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:55:06.798525

## Purpose

**Summary:** This file defines the Database Description (DBD) for DBPAUTX0, an IMS database. It specifies the database structure, including dataset groups, segments, fields, and indexing, using the INDEX access method with VSAM and password protection disabled.

**Business Context:** None
**Program Type:** UTILITY

## Paragraphs

### DBD Definition

This section defines the structure of the IMS database DBPAUTX0. It starts by declaring the DBD name, access method (INDEX, VSAM, PROT), and password protection status (NO) on line 18. The VERSION parameter is also present but has no specified value. The definition proceeds to define a single dataset group (DSG001) on line 23, specifying the DD name (DDPAUTX0) and size (4096). Following this, the segment PAUTINDX is defined on line 27, specifying its name, parent (0 indicating root segment), byte size (6), and frequency. The segment contains a single field, INDXSEQ, defined as a packed decimal (TYPE=P) sequence field, starting at position 1 with a length of 6 bytes. Finally, a logical child relationship is defined on line 30, linking PAUTINDX to PAUTSUM0 in database DBPAUTP0, using ACCNTID as the index.

## Data Flow
