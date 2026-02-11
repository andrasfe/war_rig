# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-10 17:20:36.386201

## Purpose

This file defines a Program Specification Block (PSB) named PSBPAUTL for an IMS database application. It specifies the database (DBPAUTP0) and the segments (PAUTSUM0, PAUTDTL1) that the program is sensitive to.

## Paragraphs/Procedures

### PSB Definition
This section defines the PSB (Program Specification Block) for IMS database access. The PSB specifies the database to be accessed (DBPAUTP0), the processing options (PROCOPT=L), and the key length (KEYLEN=14). It also defines the sensitive segments, PAUTSUM0 and PAUTDTL1, specifying their hierarchical relationship. The PSBGEN statement indicates that the PSB is generated for assembly language and assigns the name PSBPAUTL to the PSB. The END statement marks the end of the PSB definition.
