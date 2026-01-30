# DBPAUTP0

**File**: `ims/DBPAUTP0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-30 19:40:55.366051

## Purpose

This DBD source file defines the IMS HIDAM database DBPAUTP0 using VSAM access for pending authorization data. It specifies dataset DDPAUTP0, root segment PAUTSUM0 (100 bytes) with unique sequential key ACCNTID, and child segment PAUTDTL1 (200 bytes) with unique sequential key PAUT9CTS parented by PAUTSUM0. The file is processed by the DBDGEN utility to generate the DBD control block.

**Business Context**: Stores summary and detail records for pending account authorizations, supporting hierarchical access via root account ID.

## Outputs

| Name | Type | Description |
|------|------|-------------|
| DDPAUTP0 | IOType.FILE_VSAM | VSAM dataset hosting the HIDAM database segments |
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment containing pending authorization summary data, 100 bytes |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment containing pending authorization detail data, 200 bytes |

## Business Rules

- **BR001**: Root segment PAUTSUM0 is uniquely sequenced by ACCNTID field
- **BR002**: Child segment PAUTDTL1 is parented exclusively by root segment PAUTSUM0
- **BR003**: Child segment PAUTDTL1 is uniquely sequenced by PAUT9CTS field

## Open Questions

- ? Exact business meaning of fields ACCNTID and PAUT9CTS
  - Context: Fields named but data content not described in DBD
