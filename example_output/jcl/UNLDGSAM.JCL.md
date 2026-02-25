# UNLDGSAM

**File:** jcl/UNLDGSAM.JCL
**Type:** JCL
**Status:** In Progress
**Iterations:** 1
**Analyzed:** 2026-02-25 15:30:18.250600

## Purpose

This JCL job executes the IMS GSAM database unload utility DFSRRC00 to unload the PAUTDB database (segments ROOT and CHILD from GSAM datasets). It allocates necessary IMS libraries, DBD libraries, and control datasets for the unload process. Standard SYSOUT datasets capture listings, dumps, and errors.

**Business Context:** Supports unloading of the PAUTDB GSAM database in the AWS M2 CARDDEMO application, likely for backup, migration, or analysis purposes.
**Program Type:** BATCH
**Citations:** Lines 1, 23, 26, 36, 39

## Inputs

### PASFILOP
- **Type:** FILE_VSAM
- **Description:** PAUTDB ROOT GSAM database dataset read by the unload utility
- **Lines:** 36

### PADFILOP
- **Type:** FILE_VSAM
- **Description:** PAUTDB CHILD GSAM database dataset read by the unload utility
- **Lines:** 39

### DDPAUTP0
- **Type:** FILE_SEQUENTIAL
- **Description:** IMS PAUTHDB dataset, likely containing DBD definitions or control data for PAUTDB
- **Lines:** 42

### DDPAUTX0
- **Type:** FILE_SEQUENTIAL
- **Description:** IMS PAUTHDBX dataset, likely index or extended control data for PAUTDB
- **Lines:** 43

### DFSVSAMP
- **Type:** FILE_SEQUENTIAL
- **Description:** IMS proclib member DFSVSMDB providing VSAM macro definitions for GSAM processing
- **Lines:** 46

### STEPLIB
- **Type:** OTHER
- **Description:** Load libraries for IMS SDFSRESL (v151), and application AWS.M2.CARDDEMO.LOADLIB
- **Lines:** 28

### DFSRESLB
- **Type:** OTHER
- **Description:** Additional IMS SDFSRESL library for resource modules
- **Lines:** 31

### IMS
- **Type:** OTHER
- **Description:** IMS PSBLIB and DBDLIB for program specification blocks and database definitions
- **Lines:** 33

### PARM
- **Type:** PARAMETER
- **Description:** Execution parameters 'DLI,DBUNLDGS,DLIGSAMP,,,,,,,,,,,N' specifying DLI mode, GSAM unload function for DLIGSAMP database
- **Lines:** 27

## Outputs

### SYSPRINT
- **Type:** REPORT
- **Description:** Standard print output including unload listings and messages
- **Lines:** 50

### SYSUDUMP
- **Type:** REPORT
- **Description:** System dump output in case of abends
- **Lines:** 51

### IMSERR
- **Type:** REPORT
- **Description:** IMS error messages output
- **Lines:** 52

## Data Flow

### Reads From
- **PASFILOP**: UNKNOWN (GSAM records)
  (Lines: 36)
- **PADFILOP**: UNKNOWN (GSAM records)
  (Lines: 39)

### Writes To
- **SYSPRINT**: Unload listings and diagnostics
  (Lines: 50)

## Key Paragraphs

### UNLDGSAM
**Purpose:** This paragraph represents the overall JCL job definition named UNLDGSAM, which orchestrates the execution of the IMS GSAM database unload process. It consumes job parameters including class A, MSGCLASS H, REGION 0M, TIME 1440, and notification to SYSUID from lines 1-2. It includes extensive copyright, license, and descriptive comments from lines 3-25 explaining the purpose as 'EXECUTE IMS PROGRAM' for unloading. The job defines a single step STEP01 that executes PGM=DFSRRC00 with specific PARM for DLI DBUNLDGS on DLIGSAMP database (line 27). It allocates input datasets such as PASFILOP (ROOT GSAM), PADFILOP (CHILD GSAM), DDPAUTP0/ DDPAUTX0 (PAUTHDB control), and DFSVSAMP (VSAM macros), along with required libraries STEPLIB, DFSRESLB, and IMS PSBLIB/DBDLIB. Outputs are directed to SYSPRINT, SYSUDUMP, and IMSERR for reporting and diagnostics. There are no explicit business logic decisions or validations in the JCL itself; these are handled internally by DFSRRC00. Error handling relies on standard z/OS mechanisms with DUMMY DD's for IMSLOGR and IEFRDER to suppress unnecessary logs (lines 48-49), and SYSUDUMP captures abends. No subordinate paragraphs are called as this is JCL structure, but it effectively calls the IMS utility program DFSRRC00 to read GSAM inputs and produce unload output. No CICS or SQL operations are present.
- Lines: 1-54

## Error Handling

- **Program abend:** SYSUDUMP to SYSOUT and IMSERR to SYSOUT
  (Lines: 51)
- **IMS log requirements:** DUMMY allocation for IMSLOGR and IEFRDER
  (Lines: 48)

## Open Questions

- **What is the specific output dataset for the GSAM unload records?**
  - Context: No explicit sequential unload DD (e.g., UNLOADxx or SYSUT1) is defined; output appears limited to SYSPRINT
  - Suggestion: Review IMS DFSRRC00 DBUNLDGS documentation for default output DD names or check job logs for actual unload destination

## Resolved Questions

- **Q:** Is DLIGSAMP the DBD name for PAUTDB, or a different database?
  **A:** **No, DLIGSAMP is not the DBD name for PAUTDB (or any database).**

### Key Findings:
- **DLIGSAMP is a PSB name** (Program Specification Block), not a DBD (Database Definition). It is specified in the `UNLDGSAM.JCL` PARM for DFSRRC00:  
  ```
  //STEP01  EXEC PGM=DFSRRC00,
  //             PARM='DLI,DBUNLDGS,DLIGSAMP,,,,,,,,,,,N'
  ```
  Here, DFSRRC00 (IMS GSAM unload utility) uses:
  - `DLI`: DL/I region type.
  - `DBUNLDGS`: Utility program (`cbl/DBUNLDGS.CBL`).
  - `DLIGSAMP`: PSB name.

- **PSB Definition** (`ims/DLIGSAMP.PSB`):  
  ```
  PAUTBPCB PCB TYPE=DB,DBDNAME=DBPAUTP0,PROCOPT=GOTP,KEYLEN=14
           SENSEG NAME=PAUTSUM0,PARENT=0
           SENSEG NAME=PAUTDTL1,PARENT=PAUTSUM0
  PCB TYPE=GSAM,DBDNAME=PASFLDBD,PROCOPT=LS
  PCB TYPE=GSAM,DBDNAME=PADFLDBD,PROCOPT=LS
  PSBGEN LANG=COBOL,PSBNAME=DLIGSAMP
  ```
  - Accesses **DBPAUTP0** (hierarchical DBD, root=PAUTSUM0, child=PAUTDTL1).
  - Plus two **GSAM DBDs**: **PASFLDBD** (ROOT segment) and **PADFLDBD** (CHILD segment).

- **PAUTDB Context**:
  - "PAUTDB" is a **logical/dataset name** for GSAM files:  
    `AWS.M2.CARDDEMO.PAUTDB.ROOT.GSAM` → `PASFILOP` DD (maps to PASFLPCB/PASFLDBD).  
    `AWS.M2.CARDDEMO.PAUTDB.CHILD.GSAM` → `PADFILOP` DD (maps to PADFLPCB/PADFLDBD).
  - These are unloaded by DFSRRC00 via PSB `DLIGSAMP` + program `DBUNLDGS.CBL`.
  - `DBUNLDGS.CBL` reads hierarchical data via `PAUTBPCB` (DBPAUTP0) and writes to GSAM PCBs (`PASFLPCB`, `PADFLPCB`).

- **No DBD named DLIGSAMP or PAUTDB**:
  - Searches for `DLIGSAMP`/`PAUTDB` in `*.dbd` files: No matches.
  - Actual DBDs: `PASFLDBD` (GSAM ROOT), `PADFLDBD` (GSAM CHILD), `DBPAUTP0` (hierarchical PAU DB).

**Summary**: `UNLDGSAM.JCL` unloads **PAUTDB GSAM datasets** (ROOT/CHILD segments via DBDs `PASFLDBD`/`PADFLDBD`) using PSB `DLIGSAMP`. PAUTDB is **not** a formal DBD—it's the dataset prefix for the GSAM-accessible version of the main PAU database (DBD=`DBPAUTP0`).

**Sources**: `jcl/UNLDGSAM.JCL` (lines 26-40), `ims/DLIGSAMP.PSB` (lines 17-23), `cbl/DBUNLDGS.CBL` (PCBs/CALL 'CBLTDLI'), `cpy/PASFLPCB.CPY`/`PADFLPCB.CPY` (DBDNAME fields), skill `system-overview`.

---
*Generated by War Rig WAR_RIG*