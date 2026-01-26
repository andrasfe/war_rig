# PAUTBPCB

**File**: `cpy/PAUTBPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 02:34:10.433869

## Purpose

This copybook defines the 01-level data structure PAUTBPCB for an IMS Database Program Communication Block (DBPCB). It specifies fields used by IMS DL/I programs to communicate with the PAUT database, including database name, segment level, status, processing options, segment name, key feedback details, and the key feedback area. The structure follows standard IMS DBPCB layout conventions based on field names and PIC clauses.

**Business Context**: Supports IMS DL/I database access for the PAUT application, typically in batch or online IMS transactions requiring hierarchical database navigation.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUT-DBDNAME | IOType.OTHER | Database name (DBD), populated by IMS from the PSB definition prior to program execution; read-only by application. |
| PAUT-PCB-PROCOPT | IOType.OTHER | Processing options for the PCB, set by IMS from PSB; determines allowable DL/I call types (e.g., G, I, IS). |
| PAUT-KEYFB | IOType.OTHER | Key feedback area; application populates with segment search keys prior to qualified DL/I calls (e.g., GU, GN). |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUT-SEG-LEVEL | IOType.OTHER | Segment hierarchical level of the retrieved segment, populated by IMS after successful DL/I call. |
| PAUT-PCB-STATUS | IOType.OTHER | Status code from the most recent DL/I call, populated by IMS (application checks post-call). |
| PAUT-SEG-NAME | IOType.OTHER | Name of the segment retrieved or affected by the DL/I call, populated by IMS. |
| PAUT-KEYFB-NAME | IOType.OTHER | Key feedback name or identifier, populated by IMS. |
| PAUT-NUM-SENSEGS | IOType.OTHER | Number of sensitive segments for this PCB, populated by IMS from PSB. |
| PAUT-KEYFB | IOType.OTHER | Key feedback area updated by IMS with positioning information and keys after DL/I call. |
