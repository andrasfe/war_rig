# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-24 04:01:33.149730

## Purpose

This PSB defines a single database PCB named PAUTLPCB for accessing the IMS database DBDNAME=DBPAUTP0 with PROCOPT=L (enabling unqualified segment searches) and KEYLEN=14. It specifies two retrievable segments: PAUTSUM0 as the root segment (PARENT=0) and PAUTDTL1 as its child segment under PAUTSUM0. Application programs linked to PSBNAME=PSBPAUTL use this PCB to perform DL/I calls for segment retrieval.

**Business Context**: IMS database access interface for DBPAUTP0, likely supporting authorization or utility functions based on naming (PAUTL)

## Inputs

| Name | Type | Description |
|------|------|-------------|
| SSAs | IOType.IMS_SEGMENT | Segment Search Arguments provided by application programs for qualified or unqualified searches on PAUTSUM0 (root) and PAUTDTL1 (child) segments, limited by KEYLEN=14 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment retrievable via SSAs from DBPAUTP0 database |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment under PAUTSUM0, retrievable via SSAs from DBPAUTP0 database |

## Open Questions

- ? Exact meaning and implications of PROCOPT=L
  - Context: Standard IMS documentation not present in source; known to enable unqualified SSAs but full permissions (e.g., read-only vs. update) unclear without IMS manual reference
- ? Specific business purpose of DBPAUTP0 database and segments PAUTSUM0/PAUTDTL1
  - Context: Inferred from naming (PAUTL suggests Payment Authorization Utility) but not explicitly stated
