# PAUTBPCB

**File**: `cpy/PAUTBPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-25 15:28:48.076025

## Purpose

This copybook defines the IMS Program Communication Block (PCB) structure PAUTBPCB used for DL/I database access to the PAUTB database. It includes fields for DBD name, segment level, PCB status, processing options, segment name, key feedback length, number of sensitive segments, and the key feedback buffer. The structure matches the standard IMS DB PCB layout for program-IMS communication.

**Business Context**: IMS DL/I hierarchical database access control block for PAUTB database

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUT-DBDNAME | IOType.OTHER | Database DBD name provided by IMS control region |
| PAUT-PCB-PROCOPT | IOType.OTHER | Processing options from PSB definition, provided by IMS |
| PAUT-KEYFB-NAME | IOType.OTHER | Key feedback area name/offset provided by IMS |
| PAUT-NUM-SENSEGS | IOType.OTHER | Number of sensitive segments from PSB, provided by IMS |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUT-SEG-LEVEL | IOType.OTHER | Segment hierarchical level filled by IMS after retrieval call |
| PAUT-PCB-STATUS | IOType.OTHER | Status code set by IMS after each DL/I call |
| PAUT-SEG-NAME | IOType.OTHER | Name of the retrieved segment filled by IMS |
| PAUT-KEYFB | IOType.OTHER | Key feedback buffer populated by IMS with concatenated keys |

## Business Rules

- **BR001**: Fields must conform to defined PIC sizes and types for IMS PCB compatibility

## Paragraphs/Procedures

### ~~PAUTBPCB~~ (Dead Code)
*Record layout 'PAUTBPCB' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| PAUTBPCB | record_layout | 1 | Record layout 'PAUTBPCB' is never used by any program |
