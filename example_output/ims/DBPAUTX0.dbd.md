# DBPAUTX0

**File**: `ims/DBPAUTX0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 23:05:34.453132

## Purpose

This DBD (Database Description) file defines the structure and access methods for the DBPAUTX0 database, which uses an index and VSAM for data storage. It defines a single segment, PAUTINDX, and a dataset group DSG001, specifying the DD name and size. It also defines a logical child relationship to PAUTSUM0 in database DBPAUTP0.

## Open Questions

- ? What is the purpose of the VERSION parameter in the DBD macro?
  - Context: The code includes VERSION= without a value, and its significance is unclear.
