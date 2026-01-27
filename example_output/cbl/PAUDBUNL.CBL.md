# PAUDBUNL

**File**: `cbl/PAUDBUNL.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-27 23:04:27.591831

## Purpose

The COBOL program PAUDBUNL unloads pending authorization summary and detail segments from an IMS database to two sequential output files. It reads PAUTSUM0 root segments and PAUTDTL1 child segments, writing them to OPFILE1 and OPFILE2 respectively, after checking if the PA-ACCT-ID is numeric.

**Business Context**: This program is likely used for data migration, reporting, or archiving purposes related to pending authorizations.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| IMS Database (PAUTSUM0, PAUTDTL1) | IOType.IMS_SEGMENT | Pending authorization summary (root) and detail (child) segments from an IMS database. |
| PAUTBPCB | IOType.PARAMETER | PCB mask for IMS calls. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OPFILE1 | IOType.FILE_SEQUENTIAL | Unloaded pending authorization summary segments. |
| OPFILE2 | IOType.FILE_SEQUENTIAL | Unloaded pending authorization detail segments. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| CBLTDLI | CallType.STATIC_CALL | Performs IMS database calls to retrieve segments. |
| CBLTDLI | CallType.STATIC_CALL | Performs IMS database calls to retrieve child segments. |

## Business Rules

- **BR001**: Only write the summary record to OPFILE1 if PA-ACCT-ID is numeric.

## Paragraphs/Procedures

### MAIN-PARA
This is the main control paragraph of the program. It first calls 1000-INITIALIZE to perform initialization tasks such as accepting the current date and opening the output files OPFILE1 and OPFILE2. Then, it enters a loop that continues until WS-END-OF-ROOT-SEG is set to 'Y', indicating that all root segments have been processed. Inside the loop, it calls 2000-FIND-NEXT-AUTH-SUMMARY to retrieve and process the next root segment. After processing all root segments, it calls 4000-FILE-CLOSE to close the output files. Finally, it terminates the program with a GOBACK statement. The paragraph also includes an ENTRY point 'DLITCBL' for IMS.

### 1000-INITIALIZE
This paragraph initializes the program by accepting the current date and opening the output files OPFILE1 and OPFILE2. It retrieves the current date from the system using ACCEPT statements and stores it in CURRENT-DATE and CURRENT-YYDDD. It then opens OPFILE1 and OPFILE2 for output. If either file fails to open, the program displays an error message and calls 9999-ABEND to terminate. There is a commented-out line that suggests the program might have originally accepted parameters from SYSIN. The paragraph displays messages to the console indicating the start of the program and the current date.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph retrieves and processes the next pending authorization summary (root) segment from the IMS database. It initializes PAUT-PCB-STATUS and then calls the CBLTDLI routine with the FUNC-GN function code to retrieve the next root segment using the PAUTBPCB PCB mask, PENDING-AUTH-SUMMARY segment layout, and ROOT-UNQUAL-SSA SSA. If the PCB status is spaces, indicating a successful read, it increments counters WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves the segment data to OPFIL1-REC, and calls 3000-FIND-NEXT-AUTH-DTL to process the child segments. If the PCB status is 'GB', indicating end of database, it sets WS-END-OF-ROOT-SEG to 'Y'. If the PCB status is neither spaces nor 'GB', it displays an error message and calls 9999-ABEND to terminate the program.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph retrieves and processes the next pending authorization detail (child) segment from the IMS database for the current root segment. It calls the CBLTDLI routine with the FUNC-GNP function code to retrieve the next child segment using the PAUTBPCB PCB mask, PENDING-AUTH-DETAILS segment layout, and CHILD-UNQUAL-SSA SSA. If the PCB status is spaces, indicating a successful read, it sets MORE-AUTHS to TRUE, increments counters WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves the segment data to CHILD-SEG-REC, and writes the record to OPFILE2. If the PCB status is 'GE', indicating segment not found, it sets WS-END-OF-CHILD-SEG to 'Y'. If the PCB status is neither spaces nor 'GE', it displays an error message and calls 9999-ABEND to terminate the program. Finally, it initializes PAUT-PCB-STATUS.

### 4000-FILE-CLOSE
This paragraph closes the output files OPFILE1 and OPFILE2. It displays a message to the console indicating that the files are being closed. It then closes each file and checks the file status. If an error occurs during the close operation, it displays an error message. The paragraph does not perform any error handling beyond displaying the error message; it continues execution regardless of the file close status.

### 9999-ABEND
This paragraph handles abnormal program termination. It displays a message to the console indicating that the program is abending. It then sets the RETURN-CODE to 16 and terminates the program using the GOBACK statement. This paragraph is called when a critical error occurs, such as a file open or close error, or an IMS call failure.

## Control Flow

```mermaid
flowchart TD
    %% Title: PAUDBUNL.CBL
    1000_EXIT["1000-EXIT"]
    1000_INITIALIZE["1000-INITIALIZE"]
    9999_ABEND["9999-ABEND"]
    2000_EXIT["2000-EXIT"]
    2000_FIND_NEXT_AUTH_SUMMARY["2000-FIND-NEXT-AUTH-SUMMARY"]
    3000_FIND_NEXT_AUTH_DTL["3000-FIND-NEXT-AUTH-DTL"]
    CBLTDLI__ext(["CBLTDLI"])
    3000_EXIT["3000-EXIT"]
    4000_EXIT["4000-EXIT"]
    4000_FILE_CLOSE["4000-FILE-CLOSE"]
    9999_EXIT["9999-EXIT"]
    MAIN_PARA["MAIN-PARA"]
    1000_INITIALIZE --> 9999_ABEND
    2000_FIND_NEXT_AUTH_SUMMARY --> 3000_FIND_NEXT_AUTH_DTL
    2000_FIND_NEXT_AUTH_SUMMARY --> 9999_ABEND
    2000_FIND_NEXT_AUTH_SUMMARY -.->|calls| CBLTDLI__ext
    3000_FIND_NEXT_AUTH_DTL --> 9999_ABEND
    3000_FIND_NEXT_AUTH_DTL -.->|calls| CBLTDLI__ext
    MAIN_PARA --> 1000_INITIALIZE
    MAIN_PARA --> 2000_FIND_NEXT_AUTH_SUMMARY
    MAIN_PARA --> 4000_FILE_CLOSE
```

## Open Questions

- ? What is the purpose of the commented-out ACCEPT PRM-INFO FROM SYSIN statement?
  - Context: It is unclear if this program was originally intended to accept parameters from SYSIN, and if so, what those parameters represent.
- ? What is the exact structure of the IMS database being accessed?
  - Context: While the program accesses PAUTSUM0 and PAUTDTL1 segments, the full database schema and relationships are not clear from the code alone.
