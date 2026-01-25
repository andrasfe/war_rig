# PAUTBPCB

**File**: `cpy/PAUTBPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-25 18:39:25.121161

## Purpose

Defines the PAUTBPCB 01-level group item as an IMS Database Program Communication Block (DPCB). Includes DBD name (line 18), segment level (19), PCB status (20), processing options (21), 4-byte FILLER reserved field (22), segment name (23), key feedback name/length (24), number of sensitive segments (25), and key feedback buffer (26).

**Business Context**: Facilitates IMS DL/I database calls by providing application addressable feedback areas for status, segment identification, and key positioning.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.IMS_SEGMENT | IMS DPCB populated by IMS after DL/I calls containing status, segment name, and key feedback data read by the application |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.IMS_SEGMENT | PCB structure passed by application to IMS on DL/I calls; fields like segment level may be set by application |

## Business Rules

- **BR001**: PAUT-PCB-STATUS provides IMS status from last DL/I call; application must check for non-blank to detect errors
- **BR002**: FILLER field (line 22) is reserved per IMS DPCB layout between PROCOPT and SEG-NAME

## Open Questions

- ? Precise role of PAUT-KEYFB-NAME (line 24, PIC S9(05) COMP)
  - Context: Name suggests key feedback identifier but type is binary; may be length or offset per IMS variant
- ? Exact byte offsets and full conformance to IMS PCB mask
  - Context: PIC clauses imply standard layout but FILLER and KEYFB-NAME naming are non-standard
