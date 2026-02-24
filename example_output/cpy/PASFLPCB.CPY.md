# PASFLPCB

**File**: `cpy/PASFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-24 04:02:41.559721

## Purpose

This copybook defines the IMS Program Communication Block (PCB) structure named PASFLPCB for accessing the PASFL hierarchical database via DL/I calls. It specifies standard PCB fields including DBD name, segment level, status, processing options, segment name, key feedback name, number of sensitive segments, and key feedback area. Used in IMS programs to interface with the PASFL database.

**Business Context**: Supports IMS DL/I database navigation and access control in mainframe applications, part of Amazon-licensed mainframe code under Apache 2.0.

## Business Rules

- **BR001**: PCB Processing Options define permitted DL/I call functions for the PASFL PCB.
- **BR002**: PCB Status Code indicates result of last DL/I call on PASFL PCB.

## Paragraphs/Procedures

### ~~PASFLPCB~~ (Dead Code)
*Record layout 'PASFLPCB' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| PASFLPCB | record_layout | 1 | Record layout 'PASFLPCB' is never used by any program |
