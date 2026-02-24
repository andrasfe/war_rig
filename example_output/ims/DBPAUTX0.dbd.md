# DBPAUTX0

**File**: `ims/DBPAUTX0.dbd`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-24 03:57:03.550681

## Purpose

Defines the IMS database DBPAUTX0 as an indexed, VSAM-protected database serving as an index pointer. Contains one dataset group DSG001 mapped to DDPAUTX0 with size 4096. Features a single root segment PAUTINDX (6 bytes) sequenced by packed decimal field INDXSEQ, with a logical child to PAUTSUM0 in DBPAUTP0 via ACCNTID index.

## Business Rules

- **BR001**: Root segment PAUTINDX is defined as 6 bytes in length with a frequency of 100000 occurrences.
- **BR002**: Sequence field INDXSEQ provides unique entry sequencing for PAUTINDX segment, starting at byte 1 with 6-byte packed decimal format.
- **BR003**: PAUTINDX segment establishes logical child relationship to PAUTSUM0 segment in DBPAUTP0 database, indexed by ACCNTID.

## Open Questions

- ? What is the specific business purpose of the DBPAUTX0 index database (e.g., account indexing)?
  - Context: File contains technical definitions only; no descriptive comments on business use.
