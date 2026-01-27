# PASFLPCB

**File**: `cpy/PASFLPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 02:44:15.851827

## Purpose

This copybook defines the structure of the PASFL Program Communication Block (PCB) for IMS DL/I database access. It includes fields for DBD name, segment level, status, processing options, segment name, key feedback length, number of sensitive segments, and the key feedback buffer. Used in COBOL programs to interface with the IMS hierarchical database PASFL.

**Business Context**: IMS database navigation and access control for the PASFL database in application programs.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PASFL-PCB-PROCOPT | IOType.OTHER | Processing options field set by the application prior to DL/I calls to specify the database function (e.g., GO, GNP, K) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PASFL-PCB-STATUS | IOType.OTHER | PCB status code populated by IMS after DL/I calls indicating success or failure |
| PASFL-SEG-LEVEL | IOType.OTHER | Segment hierarchy level returned by IMS after DL/I calls |
| PASFL-SEG-NAME | IOType.OTHER | Name of the segment retrieved by IMS |
| PASFL-KEYFB-NAME | IOType.OTHER | Length of the key feedback area provided by IMS |
| PASFL-NUM-SENSEGS | IOType.OTHER | Number of sensitive segments for the PCB as defined in the PSB |
| PASFL-KEYFB | IOType.OTHER | Key feedback buffer populated by IMS with concatenated keys and parentage after qualifying DL/I calls |

## Business Rules

- **BR001**: Application programs set PASFL-PCB-PROCOPT to valid IMS processing options prior to issuing DL/I calls
- **BR002**: After DL/I calls, application programs must check PASFL-PCB-STATUS for '00' (success) or error codes
- **BR003**: Key feedback length is provided in PASFL-KEYFB-NAME for application use in navigating the key feedback area

## Open Questions

- ? Exact valid values and lengths for processing options in PASFL-PCB-PROCOPT
  - Context: Copybook defines layout (PIC X(04)) but not enumerated values; relies on IMS standards
- ? Precise usage of FILLER field at line 22
  - Context: Unnamed FILLER PIC S9(05) COMP; standard IMS PCB may use for reserved or length
