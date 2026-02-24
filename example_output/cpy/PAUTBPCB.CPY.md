# PAUTBPCB

**File**: `cpy/PAUTBPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-24 04:00:03.270320

## Purpose

This COBOL copybook defines the PAUTBPCB level 01 record layout for an IMS Database Program Communication Block (PCB). It specifies fields used in DL/I calls for database navigation and access in the PAUT IMS database. Standard IMS PCB elements include status codes, segment information, and key feedback areas.

**Business Context**: IMS DL/I database operations in batch or online programs for PAUT application data access.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUT-DBDNAME | IOType.IMS_SEGMENT | Database name (DBD) provided by IMS |
| PAUT-PCB-PROCOPT | IOType.IMS_SEGMENT | PCB processing options from PSB (e.g., GO, K) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUT-SEG-LEVEL | IOType.IMS_SEGMENT | Segment hierarchy level of qualified segment |
| PAUT-PCB-STATUS | IOType.IMS_SEGMENT | Status code from DL/I call |
| PAUT-SEG-NAME | IOType.IMS_SEGMENT | Name of the last qualified segment |
| PAUT-KEYFB-NAME | IOType.IMS_SEGMENT | Key feedback area name |
| PAUT-NUM-SENSEGS | IOType.IMS_SEGMENT | Number of sensitive segments |
| PAUT-KEYFB | IOType.IMS_SEGMENT | Key feedback bytes |

## Business Rules

- **BR001**: PCB status code in PAUT-PCB-STATUS indicates DL/I call outcome using standard IMS codes such as spaces for success or 'GE' for segment not found.
- **BR002**: Processing options in PAUT-PCB-PROCOPT control allowed DL/I verbs (e.g., G for Get, I for Insert).

## Paragraphs/Procedures

### ~~PAUTBPCB~~ (Dead Code)
*Record layout 'PAUTBPCB' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| PAUTBPCB | record_layout | 1 | Record layout 'PAUTBPCB' is never used by any program |
