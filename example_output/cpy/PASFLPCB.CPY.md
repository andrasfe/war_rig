# PASFLPCB

**File**: `cpy/PASFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-10 17:20:15.610583

## Purpose

This copybook defines the data structure PASFLPCB, which appears to be related to IMS PCB (Program Communication Block) based on the field names. It includes fields for DBD name, segment level, PCB status, processing options, segment name, key feedback, number of sensitive segments, and key feedback area. This structure is likely used for communication between a program and IMS.

## Paragraphs/Procedures

### ~~PASFLPCB~~ (Dead Code)
*Record layout 'PASFLPCB' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| PASFLPCB | record_layout | 1 | Record layout 'PASFLPCB' is never used by any program |

## Open Questions

- ? How is this copybook used in conjunction with IMS?
  - Context: The field names suggest an IMS PCB, but without seeing the program that uses it, the exact usage is unclear.
