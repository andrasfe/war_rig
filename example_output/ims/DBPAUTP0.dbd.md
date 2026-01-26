# DBPAUTP0

**File**: `ims/DBPAUTP0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 14:21:28.415527

## Purpose

This DBD source file defines the IMS database DBPAUTP0 as a HIDAM database with VSAM access and no password. It specifies one dataset group DSG001 using dataset DDPAUTP0 of size 4096 with SCAN=3. The database contains two segments: root segment PAUTSUM0 (Pending Authorization Summary, 100 bytes) with packed key field ACCNTID (positions 1-6) and a logical child to PAUTINDX in DBPAUTX0, and child segment PAUTDTL1 (Pending Authorization Details, 200 bytes) under PAUTSUM0 with character key PAUT9CTS (positions 1-8).

**Business Context**: Supports storage and retrieval of pending authorization summary and detail records, likely for financial account or transaction authorization workflows.

## Business Rules

- **BR001**: Database access method is HIDAM with VSAM, EXIT for key/data/nopath/nocascade/log, no password
- **BR002**: Root segment PAUTSUM0 (Pending Authorization Summary) is 100 bytes, unorderable sequence key ACCNTID (packed, 6 bytes at position 1), rules empty/HIERARCHY (HERE), twin backward pointer
- **BR003**: Child segment PAUTDTL1 (Pending Authorization Details) is 200 bytes under parent PAUTSUM0, unorderable sequence key PAUT9CTS (character, 8 bytes at position 1)
- **BR004**: Dataset group DSG001 uses DDNAME DDPAUTP0 with size 4096 and SCAN=3

## Open Questions

- ? What copybooks or data structures define the full layouts of segments PAUTSUM0 and PAUTDTL1 beyond the key fields?
  - Context: Only key fields are explicitly defined in the DBD; remaining bytes are not detailed here
- ? Details of the logical child PAUTINDX in database DBPAUTX0?
  - Context: LCHILD references PAUTINDX in DBPAUTX0 but no further details provided
- ? Associated PSB (Program Specification Block) for applications accessing this database?
  - Context: DBD defines database but not application access paths
