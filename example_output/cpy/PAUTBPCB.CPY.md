# PAUTBPCB

**File**: `cpy/PAUTBPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 15:15:32.838197

## Purpose

This copybook defines the PAUTBPCB 01-level record structure for an IMS Program Communication Block (PCB) used in IMS/DB applications. It specifies fields for database identification, processing options set by the application, status and feedback fields populated by IMS, and key feedback buffer. The structure follows standard IMS PCB layout for DL/I database calls.

**Business Context**: IMS database access control block for PAUT application, enabling application programs to issue DL/I calls (GU, GNP, etc.) to hierarchical databases.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUT-DBDNAME | IOType.IMS_SEGMENT | 8-character Database Definition (DBD) name, set by the application before IMS DL/I calls |
| PAUT-PCB-PROCOPT | IOType.IMS_SEGMENT | 4-character IMS processing options (PROCOPT), set by the application to specify call types like retrieval or update |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUT-SEG-LEVEL | IOType.IMS_SEGMENT | 2-character segment level of the segment retrieved or affected by IMS call, populated by IMS |
| PAUT-PCB-STATUS | IOType.IMS_SEGMENT | 2-character status code returned by IMS (spaces=success, non-spaces=error), populated by IMS after each call |
| PAUT-SEG-NAME | IOType.IMS_SEGMENT | 8-character name of the segment retrieved, populated by IMS |
| PAUT-KEYFB-NAME | IOType.IMS_SEGMENT | Field offset/name for key feedback, populated/used by IMS |
| PAUT-NUM-SENSEGS | IOType.IMS_SEGMENT | Number of sensitive segments for the PCB, populated by IMS |
| PAUT-KEYFB | IOType.IMS_SEGMENT | 255-character key feedback buffer containing concatenated keys of parent segments up to the retrieved segment, populated by IMS |

## Business Rules

- **BR001**: Application programs must populate PAUT-DBDNAME with the target database DBD name prior to IMS DL/I calls
- **BR002**: Application programs must set PAUT-PCB-PROCOPT to appropriate IMS processing options before calls
- **BR003**: After each IMS DL/I call, application must check PAUT-PCB-STATUS for success (spaces) or error
