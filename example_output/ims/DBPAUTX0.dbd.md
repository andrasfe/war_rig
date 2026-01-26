# DBPAUTX0

**File**: `ims/DBPAUTX0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 15:14:37.504976

## Purpose

This file is an IMS Database Definition (DBD) source for the database DBPAUTX0, specifying an indexed VSAM-accessible database with protection. It defines one dataset group DSG001 referencing physical dataset DDPAUTX0 of size 4096. It includes a single root segment PAUTINDX with a unique sequential packed decimal key field INDXSEQ and a logical child relationship to PAUTSUM0 in database DBPAUTP0 via the ACCNTID index.

## Business Rules

- **BR001**: The root segment PAUTINDX enforces uniqueness and sequential ordering via the INDXSEQ key field.
- **BR002**: Establishes a logical hierarchical relationship from the PAUTINDX segment to the PAUTSUM0 segment in database DBPAUTP0.
