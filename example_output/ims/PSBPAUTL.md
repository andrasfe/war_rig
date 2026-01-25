# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-25 18:35:16.172519

## Purpose

Defines the PSB named PSBPAUTL for IMS DL/I application programs. Specifies a single DB PCB for database DBDNAME=DBPAUTP0 with PROCOPT=L (language interface supporting all DL/I operations) and KEYLEN=14. Grants SENSory access to root segment PAUTSUM0 and child segment PAUTDTL1 parented by PAUTSUM0.

**Business Context**: IMS database access utility (inferred from PSBPAUTL and DBPAUTP0 naming)

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) of DBPAUTP0 database |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment of PAUTSUM0 in DBPAUTP0 database |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment of DBPAUTP0; writable via ISRT/REPL/DLET due to PROCOPT=L |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment of PAUTSUM0; writable via ISRT/REPL/DLET due to PROCOPT=L |

## Business Rules

- **BR001**: Hierarchical access: PAUTDTL1 segment accessible only as child of parent PAUTSUM0
- **BR002**: Root segment PAUTSUM0 accessible directly (PARENT=0)
- **BR003**: Full DL/I authority granted (retrieve, insert, replace, delete)

## Open Questions

- ? What are the field layouts for segments PAUTSUM0 and PAUTDTL1?
  - Context: PSB defines only segment names and hierarchy; no DBD or segment layouts provided
- ? What application programs reference this PSB?
  - Context: PSB is used by IMS DL/I programs but not specified here
