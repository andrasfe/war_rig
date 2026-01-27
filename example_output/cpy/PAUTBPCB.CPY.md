# PAUTBPCB

**File**: `cpy/PAUTBPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 02:43:40.680332

## Purpose

This copybook defines the 01 PAUTBPCB level structure for an IMS Database Program Communication Block (DBPCB). It includes fields for database name, segment level, PCB status, processing options, segment name, key feedback name, number of sensitive segments, and the key feedback area. Used in IMS DL/I programs to pass control information to and receive feedback from IMS database calls.

**Business Context**: Supports IMS DL/I database access in batch or online IMS applications for segment retrieval, insertion, replacement, and deletion operations.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUT-PCB-PROCOPT | IOType.OTHER | Processing options set by the application program to specify the type of DL/I call (e.g., GU for Get Unique, GNP for Get Next, ISRT for Insert) |
| PAUT-DBDNAME | IOType.OTHER | Database name, typically predefined in the PSB and referenced or confirmed by the application |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUT-PCB-STATUS | IOType.OTHER | Two-byte status code returned by IMS indicating success (spaces) or error conditions after DL/I call |
| PAUT-SEG-LEVEL | IOType.OTHER | Segment level of the retrieved or affected segment, populated by IMS on successful calls |
| PAUT-SEG-NAME | IOType.OTHER | Name of the segment retrieved or affected, populated by IMS |
| PAUT-KEYFB-NAME | IOType.OTHER | Key feedback name or length indicator populated by IMS |
| PAUT-NUM-SENSEGS | IOType.OTHER | Number of sensitive segments for the PCB, populated by IMS |
| PAUT-KEYFB | IOType.OTHER | Key feedback area containing concatenated key fields of the retrieved segment |

## Business Rules

- **BR001**: Application must set valid processing option codes in PAUT-PCB-PROCOPT before issuing a DL/I call
- **BR002**: After DL/I call, application must check PAUT-PCB-STATUS for success (blanks) or error codes
