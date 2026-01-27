# PSBPAUTB

**File**: `ims/PSBPAUTB.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 23:05:40.583535

## Purpose

This PSB defines the program view for accessing the DBPAUTP0 database. It specifies the program's access rights (PROCOPT=AP), key length (KEYLEN=14), and the segments it can access (PAUTSUM0, PAUTDTL1). The PSBGEN statement indicates it's designed for COBOL and compatibility is enabled.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | The DBPAUTP0 database, accessed via the PCB. |

## Open Questions

- ? What is the purpose of the PAUTSUM0 and PAUTDTL1 segments?
  - Context: The code only defines the segments, but not their content or relationship.
