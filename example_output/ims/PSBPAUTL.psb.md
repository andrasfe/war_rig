# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 17:40:13.573635

## Purpose

This PSB defines a single database PCB (PAUTLPCB) for accessing the IMS database DBPAUTP0 in browse mode (PROCOPT=L) with a key length of 14 bytes. It specifies two sensitive segments: PAUTSUM0 as the root segment (PARENT=0) and PAUTDTL1 as a dependent child segment of PAUTSUM0. The PSB is generated for assembly language programs under the name PSBPAUTL.

**Business Context**: IMS database access specification for PAUTL utility application

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | Root segment (PARENT=0) accessible via PAUTLPCB PCB |
| PAUTDTL1 | IOType.IMS_SEGMENT | Child segment dependent on parent PAUTSUM0, accessible via PAUTLPCB PCB |

## Business Rules

- **BR001**: Defines allowable access path to root sensitive segment PAUTSUM0 with no parent (PARENT=0)
- **BR002**: Defines allowable access path to child sensitive segment PAUTDTL1 under parent PAUTSUM0
- **BR003**: Restricts PCB processing option to browse-only (PROCOPT=L) with key length 14

## Open Questions

- ? What specific fields are defined in segments PAUTSUM0 and PAUTDTL1?
  - Context: PSB defines segments but not field-level details; these are in DBD/PSB source or generated macros
- ? Which application programs reference this PSB (PSBPAUTL)?
  - Context: PSB is a specification used by IMS DL/I application programs, but callers not listed in this file
