# IMSFUNCS - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** IMSFUNCS
- **File Name:** cpy/IMSFUNCS.cpy
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:47:11.422402

## Purpose

**Summary:** This COBOL copybook defines the 01-level group FUNC-CODES containing multiple 05-level elementary items, each a PIC X(04) field initialized with specific 4-character values representing IMS database function codes (e.g., 'GU  ', 'GHU ', 'GN  ', 'REPL', 'ISRT', 'DLET'). It also defines PARMCOUNT as PIC S9(05) VALUE +4 COMP-5, likely specifying the parameter count for IMS calls. These structures are used in programs performing IMS database operations such as Get Unique (GU), Get Hold Unique (GHU), Get Next (GN), Replace (REPL), Insert (ISRT), and Delete (DLET).

**Business Context:** IMS hierarchical database call parameters
**Program Type:** UTILITY

## Paragraphs

### IMSFUNCS

[Citadel] Paragraph identified by static analysis

### FUNC-CODES

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Dead Code

- **FUNC-CODES** (record_layout): Record layout 'FUNC-CODES' is never used by any program
