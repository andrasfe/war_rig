# DBPAUTX0

**File**: `ims/DBPAUTX0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-03 21:07:52.007003

## Purpose

This file defines the Database Description (DBD) for DBPAUTX0, specifying the database structure, access method, segment definitions, and field definitions. It defines an INDEX database with VSAM access and protection, including a segment named PAUTINDX and a logical child relationship to PAUTSUM0 in DBPAUTP0.

## Paragraphs/Procedures

### DBD Definition
This section defines the database characteristics. It starts with the DBD statement (line 18) specifying the database name (DBPAUTX0), access method (INDEX, VSAM, PROT), and password protection (NO). It also includes a VERSION parameter (line 19) whose value is not specified in this snippet. The DATASET group (DSG001) is defined with DD name DDPAUTX0 and a size of 4096 (line 23). The segment PAUTINDX is defined with a length of 6 bytes and a frequency of 100000 (lines 27-28). The field INDXSEQ is defined as a packed decimal field of 6 bytes starting at position 1 (line 29). Finally, a logical child relationship is established with PAUTSUM0 in database DBPAUTP0, using ACCNTID as the index (lines 30-31). The DBDGEN and FINISH statements (lines 32-33) mark the end of the DBD definition.

## Open Questions

- ? What is the purpose of the VERSION parameter on line 19?
  - Context: The value of the VERSION parameter is not specified in the provided code snippet.
