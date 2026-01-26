# DBPAUTP0

**File**: `ims/DBPAUTP0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 02:31:02.367382

## Purpose

This file is the source for the IMS Database Definition (DBD) of DBPAUTP0, defining a HIDAM database with VSAM access and EXIT options for key, data, logging without path or cascade. It specifies dataset group DSG001 with DDNAME DDPAUTP0 (4096 bytes, SCAN=3) and two segments: root segment PAUTSUM0 (100 bytes) with sequential unique key field ACCNTID (packed, 6 bytes starting at 1) and logical child PAUTINDX to DBPAUTX0, plus child segment PAUTDTL1 (200 bytes under PAUTSUM0) with sequential unique key PAUT9CTS (character, 8 bytes starting at 1). The DBD is generated via DBDGEN.

**Business Context**: Defines structure for pending authorization summary (PAUTSUM0) and detail (PAUTDTL1) records keyed by account ID, supporting hierarchical IMS database access for authorization processing.

## Business Rules

- **BR001**: Root segment PAUTSUM0 uses RULES=(,HERE) indicating no parent rules and positioning at current segment.

## Open Questions

- ? Details of referenced database DBPAUTX0 and segment PAUTINDX
  - Context: LCHILD specifies POINTER=INDX to DBPAUTX0 but no further definition in this DBD
- ? Purpose and content of fields beyond keys (e.g., remaining bytes in segments)
  - Context: Only key fields defined; other fields implied but not named
