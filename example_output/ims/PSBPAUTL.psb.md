# PSBPAUTL

**File**: `ims/PSBPAUTL.psb`
**Type**: FileType.OTHER
**Analyzed**: 2026-03-16 20:00:03.275463

## Purpose

This PSB (Program Specification Block) defines the interface for an IMS (Information Management System) application. It specifies the database (DBPAUTP0) to be accessed, the processing options (PROCOPT=L), the key length (KEYLEN=14), and the segments (PAUTSUM0, PAUTDTL1) that the application is sensitive to. It's generated for ASSEMBLY language.

## Paragraphs/Procedures

### PSB Definition
This section defines the PSB named PSBPAUTL, which is used to define the interface between an IMS application program and the IMS database. It specifies the database to be accessed (DBPAUTP0), the processing options (PROCOPT=L, indicating read-only access), and the key length (KEYLEN=14) for accessing the database. The PSB also defines the sensitive segments (SENSEG) that the application program is allowed to access. PAUTSUM0 is defined as a root segment (PARENT=0), and PAUTDTL1 is defined as a child segment of PAUTSUM0. The PSBGEN statement specifies that the PSB is generated for ASSEMBLY language.
