# PSBPAUTB

**File**: `ims/PSBPAUTB.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-24 17:38:46.647366

## Purpose

This file defines a Program Specification Block (PSB) named PSBPAUTB for an IMS database DBPAUTP0. It specifies the database access characteristics, segment hierarchy, and language interface for COBOL programs accessing this database.

## Business Rules

- **BR001**: The key length for accessing the DBPAUTP0 database is 14 bytes.

## Open Questions

- ? What is the purpose of the PAUTSUM0 and PAUTDTL1 segments?
  - Context: The SENSEG statements define segments, but their specific function within the database is unclear from this PSB definition alone.
