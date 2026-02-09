# DLIGSAMP

**File**: `ims/DLIGSAMP.PSB`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-09 15:47:53.369984

## Purpose

This PSB (Program Specification Block) defines the database access and processing options for an IMS (Information Management System) application. It specifies the PCBs (Program Communication Blocks) required for database interaction, including DB PCBs for accessing database segments and GSAM PCBs for accessing sequential datasets. The PSBGEN macro generates the PSB.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| DBPAUTP0 | IOType.IMS_SEGMENT | Database described by DBD DBPAUTP0, accessed via a DB PCB. |
| PASFLDBD | IOType.FILE_SEQUENTIAL | Sequential dataset described by DBD PASFLDBD, accessed via a GSAM PCB. |
| PADFLDBD | IOType.FILE_SEQUENTIAL | Sequential dataset described by DBD PADFLDBD, accessed via a GSAM PCB. |

## Paragraphs/Procedures

### PSBGEN
The PSBGEN statement generates the Program Specification Block (PSB) for the IMS application. It specifies the programming language as COBOL and assigns the name DLIGSAMP to the PSB. The CMPAT=NO parameter indicates that compatibility checking with previous PSB versions is disabled. The PSBGEN macro is the final step in defining the PSB, creating a load module that IMS uses to manage database access for the application program. The PSBGEN statement does not directly process data or implement business logic but rather defines the structure and access methods for data used by the application. It consumes the definitions of PCBs and SENSEGs to create the PSB. The PSBGEN statement does not handle errors directly, but errors in the PCB or SENSEG definitions will cause the PSBGEN process to fail. This paragraph does not call any other paragraphs or programs; it is a declarative statement.

## Open Questions

- ? What is the purpose of the PAUTBPCB PCB definition?
  - Context: The purpose is not clear from the provided code snippet.
- ? What is the purpose of the SENSEG definitions?
  - Context: The purpose is not clear from the provided code snippet.
