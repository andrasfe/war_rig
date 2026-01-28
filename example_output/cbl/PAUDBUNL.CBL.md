# PAUDBUNL

**File**: `cbl/PAUDBUNL.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-28 14:53:41.345925

## Purpose

The COBOL program PAUDBUNL unloads pending authorization summary and detail segments from an IMS database to two sequential output files. It reads PAUTSUM0 root segments and PAUTDTL1 child segments from the IMS database and writes them to OPFILE1 and OPFILE2 respectively, provided the PA-ACCT-ID is numeric.

**Business Context**: This program is likely used for data extraction or migration of pending authorization data from an IMS system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| IMS Database (PAUTSUM0, PAUTDTL1 Segments) | IOType.IMS_SEGMENT | Pending authorization summary (PAUTSUM0) and detail (PAUTDTL1) segments from an IMS database. |
| PAUTBPCB | IOType.PARAMETER | PCB mask for IMS calls. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OPFILE1 | IOType.FILE_SEQUENTIAL | Sequential file containing pending authorization summary records (PAUTSUM0) extracted from the IMS database.  Each record is 100 bytes long. |
| OPFILE2 | IOType.FILE_SEQUENTIAL | Sequential file containing pending authorization detail records (PAUTDTL1) extracted from the IMS database. Each record contains a ROOT-SEG-KEY (PA-ACCT-ID) and the CHILD-SEG-REC. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| CBLTDLI | CallType.STATIC_CALL | Performs IMS database calls to retrieve segments. |
| CBLTDLI | CallType.STATIC_CALL | Performs IMS database calls to retrieve child segments. |

## Business Rules

- **BR001**: Only write summary and detail records to output files if the PA-ACCT-ID is numeric.

## Paragraphs/Procedures

### MAIN-PARA
This is the main control paragraph of the program. It first calls 1000-INITIALIZE to perform initial setup tasks such as accepting the current date and opening the output files OPFILE1 and OPFILE2.  Then, it enters a loop that repeatedly calls 2000-FIND-NEXT-AUTH-SUMMARY to read and process pending authorization summary segments from the IMS database. The loop continues until the end of the database is reached, indicated by WS-END-OF-ROOT-SEG being set to 'Y'. After processing all summary segments, it calls 4000-FILE-CLOSE to close the output files. Finally, the program terminates using GOBACK.

### 1000-INITIALIZE
This paragraph initializes the program by performing several setup tasks. It accepts the current date and day from the system. It then opens the output files OPFILE1 and OPFILE2 for writing. It checks the file status after each OPEN operation. If the file status indicates an error, it displays an error message and calls 9999-ABEND to terminate the program.  The paragraph also displays the starting program message and the current date to the console.

### 2000-FIND-NEXT-AUTH-SUMMARY
This paragraph retrieves the next pending authorization summary segment (PAUTSUM0) from the IMS database. It first initializes PAUT-PCB-STATUS. It then calls the CBLTDLI routine with the FUNC-GN function code to retrieve the next summary segment using an unqualified SSA. If the PCB status is spaces (successful read), it increments counters for summary records read and processed, moves the summary segment data to OPFIL1-REC, initializes ROOT-SEG-KEY and CHILD-SEG-REC, moves PA-ACCT-ID to ROOT-SEG-KEY, and writes the record to OPFILE1 if PA-ACCT-ID is numeric. It then calls 3000-FIND-NEXT-AUTH-DTL to process the child segments. If the PCB status is 'GB' (end of database), it sets the END-OF-AUTHDB flag to TRUE and WS-END-OF-ROOT-SEG to 'Y'. If the PCB status indicates an error, it displays error messages and calls 9999-ABEND.

### 3000-FIND-NEXT-AUTH-DTL
This paragraph retrieves the next pending authorization detail segment (PAUTDTL1) for the current summary segment from the IMS database. It calls CBLTDLI with FUNC-GNP to get the next child segment. If the PCB status is spaces, it sets MORE-AUTHS to TRUE, increments counters, moves the detail segment data to CHILD-SEG-REC, and writes the record to OPFILE2. If the PCB status is 'GE' (end of child segments for the current root), it sets WS-END-OF-CHILD-SEG to 'Y'. If the PCB status indicates an error, it displays error messages and calls 9999-ABEND. Finally, it initializes PAUT-PCB-STATUS.

### 4000-FILE-CLOSE
This paragraph closes the output files OPFILE1 and OPFILE2. It displays a message indicating that the files are being closed. It then closes each file and checks the file status. If the file status indicates an error during the close operation, it displays an error message. This paragraph ensures that the output files are properly closed after processing is complete.

### 9999-ABEND
This paragraph handles abnormal program termination. It displays a message indicating that the program is abending. It sets the RETURN-CODE to 16, indicating an error condition, and then terminates the program using GOBACK.

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

- ? What are the specific selection criteria for extracting data from the IMS database beyond the PA-ACCT-ID being numeric?
  - Context: The program uses unqualified SSAs, suggesting that all segments are retrieved. However, there might be implicit selection criteria within the called IMS subroutines or database configuration.
- ? What is the purpose of the parameters defined in PRM-INFO?
  - Context: The code contains commented out ACCEPT statement for PRM-INFO. The fields P-EXPIRY-DAYS, P-CHKP-FREQ, P-CHKP-DIS-FREQ, and P-DEBUG-FLAG are not used in the program.
