# PAUDBLOD

**File**: `cbl/PAUDBLOD.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-27 02:40:33.888798

## Purpose

This batch COBOL program loads pending authorization data into an IMS hierarchical database. It reads root segment records from INFILE1, inserts them as PAUTSUM0 segments via IMS ISRT calls, then reads child records from INFILE2 (each prefixed with parent root key), performs GU on the parent root to establish hierarchy, and inserts child PAUTDTL1 segments via ISRT. It handles duplicates by tolerating 'II' status and abends on other IMS errors or file I/O failures.

**Business Context**: Supports loading of pending authorization summary and detail records into IMS for account authorization processing.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | IOType.FILE_SEQUENTIAL | Sequential file containing fixed-length 100-byte root segment records for PAUTSUM0 pending authorization summaries. |
| INFILE2 | IOType.FILE_SEQUENTIAL | Sequential file containing root segment key (S9(11) COMP-3) followed by 200-byte child segment records for PAUTDTL1 pending authorization details. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTSUM0 | IOType.IMS_SEGMENT | IMS root segment for pending authorization summaries, inserted unconditionally after reading from INFILE1. |
| PAUTDTL1 | IOType.IMS_SEGMENT | IMS child segment for pending authorization details, inserted under parent PAUTSUM0 after GU parent and reading from INFILE2. |

## Business Rules

- **BR001**: Process child segment only if ROOT-SEG-KEY is numeric to ensure valid parent key.
- **BR002**: Tolerate IMS insert status ' ' (success) or 'II' (duplicate), abend on other statuses.
- **BR003**: Require successful parent GU (status spaces or 'II') before child insert.

## Paragraphs/Procedures

### MAIN-PARA
This is the primary entry point and orchestration paragraph controlling the entire program flow for IMS database loading. It begins with an alternate ENTRY 'DLITCBL' using the PAUTBPCB linkage for potential dynamic invocation, followed by a startup display. It then performs 1000-INITIALIZE to open input files and set up dates. Next, it enters a loop performing 2000-READ-ROOT-SEG-FILE until END-ROOT-SEG-FILE is 'Y', loading all root segments first to ensure parents exist. After exhausting roots, it loops on 3000-READ-CHILD-SEG-FILE until END-CHILD-SEG-FILE is 'Y', processing children with parent GU for hierarchy. Finally, it performs 4000-FILE-CLOSE to shut down files and GOBACKs with return code intact. No explicit error handling here beyond subordinate abends; assumes all processing succeeds or abends propagate. It consumes IMS PCB from linkage and file inputs indirectly via performs. Produces IMS segments as outputs via subordinate inserts.

### 1000-INITIALIZE
This initialization paragraph sets up program environment by accepting current date and YYDDD into WS variables for potential logging. It displays startup information including today's date. It opens INFILE1 input sequential file and checks WS-INFIL1-STATUS for spaces or '00'; if not, displays error and performs 9999-ABEND. Similarly opens INFILE2 and validates its status with same error handling. It consumes no prior data beyond linkage PCBs. It produces open files ready for reads and initialized date fields. Business logic enforces file availability before proceeding to load. Error handling abends immediately on open failures to prevent invalid processing. No subordinate calls. Flow returns to 1000-EXIT.

### 2000-READ-ROOT-SEG-FILE
This paragraph handles reading and initial processing of root segment input from INFILE1 in the main loop. It performs a READ INFILE1 into INFIL1-REC. If WS-INFIL1-STATUS is spaces or '00', moves entire record to PENDING-AUTH-SUMMARY structure and performs 2100-INSERT-ROOT-SEG to load into IMS. If status '10', sets END-ROOT-SEG-FILE to 'Y' to exit loop. Other statuses trigger error display but no abend here (handled in insert). Consumes sequential file records. Produces populated root segment structure for insert. Logic decides loop continuation based on EOF. Error handling defers to subordinate for IMS issues. Calls 2100-INSERT-ROOT-SEG for each valid record.

### 2100-INSERT-ROOT-SEG
This paragraph inserts a single root segment into IMS database unconditionally. It issues CALL 'CBLTDLI' with FUNC-ISRT using PAUTBPCB, PENDING-AUTH-SUMMARY data, and unqualified ROOT-UNQUAL-SSA. Displays boundaries for debugging. Checks PAUT-PCB-STATUS: displays success if spaces, notes duplicate if 'II'. Abends via 9999-ABEND if neither spaces nor 'II' (e.g., other errors like GB, GE). Consumes pre-moved PENDING-AUTH-SUMMARY from read. Produces inserted PAUTSUM0 segment in IMS or tolerates dup. Business logic allows idempotent reloads via duplicate tolerance. Robust error handling abends on non-successful inserts. No further calls.

### 3000-READ-CHILD-SEG-FILE
This paragraph reads and prepares child segments from INFILE2 in the main post-root loop. Performs READ INFILE2 into INFIL2-REC structure. If WS-INFIL2-STATUS spaces or '00', checks if ROOT-SEG-KEY IS NUMERIC; if yes, moves key to QUAL-SSA-KEY-VALUE for qualified parent search, moves CHILD-SEG-REC to PENDING-AUTH-DETAILS, and performs 3100-INSERT-CHILD-SEG. If EOF '10', sets END-CHILD-SEG-FILE 'Y'. Other statuses display error but defer abend. Consumes child file records with parent key prefix. Produces populated child structure and SSA key. Logic validates key numeric to skip invalids. Errors handled in subordinate. Calls 3100 for valid numerics.

### 3100-INSERT-CHILD-SEG
This paragraph establishes parentage and delegates child insert for hierarchy. Initializes PAUT-PCB-STATUS, then CALL 'CBLTDLI' FUNC-GU using PAUTBPCB, PENDING-AUTH-SUMMARY (io area), and qualified ROOT-QUAL-SSA with key from child input. Displays for debug. If status spaces, displays success. Regardless, performs 3200-INSERT-IMS-CALL to insert child. Post-GU, if PAUT-PCB-STATUS not spaces and not 'II', displays failure details including KEYFB and abends. Consumes child details and parent key in SSA. Produces positioned parent for child insert. Logic enforces parent existence before child add. Error handling abends on GU failure (e.g., GE not found).

### 3200-INSERT-IMS-CALL
This paragraph performs the actual IMS insert for child segment under positioned parent. Issues CALL 'CBLTDLI' FUNC-ISRT with PAUTBPCB, PENDING-AUTH-DETAILS data, and unqualified CHILD-UNQUAL-SSA. Checks PAUT-PCB-STATUS post-call: success display if spaces, duplicate note if 'II', else displays error with KEYFB and abends via 9999. Consumes pre-GU positioned PCB and child data. Produces inserted PAUTDTL1 child segment. Business logic mirrors root insert for idempotency. Strict error handling abends non-success/dup. No further calls.

### 4000-FILE-CLOSE
This termination paragraph closes input files post-processing. Displays closing message, then CLOSE INFILE1 and checks WS-INFIL1-STATUS (spaces/'00' ok, else display error no abend). Similarly CLOSE INFILE2 with status check. Consumes opened files. Produces closed files and potential error logs. No decisions beyond status display. Error handling logs but continues to GOBACK. No calls.

### 9999-ABEND
Universal abend handler invoked on file/IMS errors. Displays 'IMS LOAD ABENDING ...' message. Sets RETURN-CODE to 16 indicating failure. Performs GOBACK to terminate. Consumes error context from caller (statuses displayed prior). Produces non-zero return code for JCL. No conditions checked here. No validation beyond invocation. No calls.

## Open Questions

- ? Exact field layouts and key fields in copybooks CIPAUSMY and CIPAUDTY?
  - Context: Copybooks define segment structures but contents not provided; cannot detail specific data transforms or validations.
- ? IMS database name and PSB name in use?
  - Context: PAUTBPCB implies PAUT database; PSB 'IMSUNLOD' commented out (line 95); not explicitly used.
- ? Purpose and population of PRM-INFO parameters (e.g., P-EXPIRY-DAYS)?
  - Context: Defined in WS (129-139) but never referenced or populated.
