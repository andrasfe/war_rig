# PAUTBPCB - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** PAUTBPCB
- **File Name:** cpy/PAUTBPCB.CPY
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:48:44.774204

## Purpose

**Summary:** This copybook defines the data structure PAUTBPCB (line 17), which is the Program Communication Block (PCB) for the PAUT IMS database. It includes fields for database name (PAUT-DBDNAME, line 18), current segment level (PAUT-SEG-LEVEL, line 19), PCB status code (PAUT-PCB-STATUS, line 20), processing options (PAUT-PCB-PROCOPT, line 21), segment name (PAUT-SEG-NAME, line 23), key feedback name offset (PAUT-KEYFB-NAME, line 24), number of sensitive segments (PAUT-NUM-SENSEGS, line 25), and a 255-byte key feedback buffer (PAUT-KEYFB, line 26). This structure is used by IMS DL/I programs to manage database calls and retrieve status and positioning information.

**Business Context:** IMS DL/I hierarchical database navigation and access for the PAUT database
**Program Type:** UTILITY

## Paragraphs

### PAUTBPCB

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Dead Code

- **PAUTBPCB** (record_layout): Record layout 'PAUTBPCB' is never used by any program
