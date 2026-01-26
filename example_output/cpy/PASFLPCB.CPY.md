# PASFLPCB

**File**: `cpy/PASFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 17:40:50.363983

## Purpose

This copybook defines the data structure for the PASFL IMS Program Communication Block (PCB), used in the LINKAGE SECTION of IMS DL/I application programs. It specifies fields for database identification, processing options set by the application, status and segment details returned by IMS, and a key feedback buffer. The structure follows standard IMS PCB layout for facilitating database calls to the PASFL database.

**Business Context**: Supports IMS DL/I database access for the PASFL database in batch or online IMS regions (MPP, BMP, etc.).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFL-PCB-PROCOPT | IOType.PARAMETER | Processing options set by the application program to specify the type of DL/I operation (e.g., GU for Get Unique, GN for Get Next, ISRT for Insert). |
| PASFL-DBDNAME | IOType.OTHER | Database name identifier (PASFL) referenced by the application for the target database. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFL-PCB-STATUS | IOType.RETURN_CODE | Two-byte status code set by IMS after a DL/I call (e.g., '00' success, 'GE' segment not found). |
| PASFL-SEG-LEVEL | IOType.OTHER | Segment hierarchical level set by IMS for the accessed segment. |
| PASFL-SEG-NAME | IOType.OTHER | Eight-byte segment name set by IMS for the accessed or sensed segment. |
| PASFL-KEYFB | IOType.OTHER | Key feedback buffer (100 bytes) filled by IMS with concatenated keys from root to accessed segment when PROCOPT requests it. |
| PASFL-NUM-SENSEGS | IOType.OTHER | Number of sensitive segments set by IMS. |

## Business Rules

- **BR001**: Application sets processing options in PCB before issuing DL/I call
- **BR002**: IMS sets PCB status code after DL/I call completion
- **BR003**: IMS populates segment level, name, and key feedback for qualified calls
