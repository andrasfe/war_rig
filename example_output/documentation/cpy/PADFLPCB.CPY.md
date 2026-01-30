# PADFLPCB

**File**: `cpy/PADFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-30 19:36:38.302209

## Purpose

This copybook defines the PADFLPCB data structure, which represents the Program Communication Block (PCB) for the PADFL database in an IMS (Information Management System) environment. It includes standard IMS PCB fields such as the database name (PADFL-DBDNAME), current segment level (PADFL-SEG-LEVEL), PCB status (PADFL-PCB-STATUS), processing options (PADFL-PCB-PROCOPT), segment name (PADFL-SEG-NAME), key feedback length (PADFL-KEYFB-NAME), number of sensitive segments (PADFL-NUM-SENSEGS), and the key feedback buffer (PADFL-KEYFB). This structure is referenced in the LINKAGE SECTION of IMS DL/I programs to facilitate database navigation, segment retrieval, and status checking during GU, GN, GNP, and other IMS calls.

## Paragraphs/Procedures

### ~~PADFLPCB~~ (Dead Code)
*Record layout 'PADFLPCB' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| PADFLPCB | record_layout | 1 | Record layout 'PADFLPCB' is never used by any program |
