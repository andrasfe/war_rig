# PADFLPCB

**File**: `cpy/PADFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-25 15:32:43.048068

## Purpose

This COBOL copybook defines the PADFLPCB level-01 group item, which maps the standard IMS Database Program Control Block (DBPCB) structure. It provides named fields for accessing IMS-provided PCB data such as database name, segment level, status codes, processing options, segment feedback, and key feedback information used in DL/I database calls.

**Business Context**: IMS DL/I hierarchical database access in mainframe batch or online programs, enabling programs to retrieve feedback from IMS after database calls like GU, GN, ISRT, etc.

## Paragraphs/Procedures

### ~~PADFLPCB~~ (Dead Code)
*Record layout 'PADFLPCB' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| PADFLPCB | record_layout | 1 | Record layout 'PADFLPCB' is never used by any program |
