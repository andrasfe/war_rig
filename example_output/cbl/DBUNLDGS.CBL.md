# DBUNLDGS

**File**: `cbl/DBUNLDGS.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-02-24 17:43:44.226557

## Purpose

The COBOL program DBUNLDGS is designed to unload data from an IMS database. It retrieves pending authorization summary and detail segments and writes them to output files. The program initializes, retrieves summary records, and then closes the files upon completion.

**Business Context**: UNKNOWN

## Inputs

| Name | Type | Description |
|------|------|-------------|
| IMS Database | IOType.IMS_SEGMENT | Pending authorization summary and detail segments from an IMS database. |
| PAUTBPCB | IOType.PARAMETER | Pending Authorization PCB mask. |
| PASFLPCB | IOType.PARAMETER | Pending Authorization Summary File PCB mask. |
| PADFLPCB | IOType.PARAMETER | Pending Authorization Detail File PCB mask. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OPFILE1 | IOType.FILE_SEQUENTIAL | Output file containing data from the IMS database. The structure is defined by OPFIL1-REC. |
| OPFILE2 | IOType.FILE_SEQUENTIAL | Output file containing data from the IMS database. The structure is defined by OPFIL2-REC, containing a root segment key and a child segment record. |

## Business Rules

- **BR001**: The program continues processing as long as the end-of-root-segment flag is not set to 'Y'.

## Paragraphs/Procedures

### MAIN-PARA
> [Source: MAIN-PARA.cbl.md](DBUNLDGS.CBL.d/MAIN-PARA.cbl.md)
This is the main paragraph of the DBUNLDGS program. It serves as the entry point and orchestrates the overall program flow. It first performs the 1000-INITIALIZE paragraph to set up the program environment, including accepting the current date and displaying program start messages. Then, it enters a loop, repeatedly performing the 2000-FIND-NEXT-AUTH-SUMMARY paragraph to retrieve and process pending authorization summary records from the IMS database. The loop continues until the WS-END-OF-ROOT-SEG flag is set to 'Y', indicating that all summary records have been processed. Finally, it performs the 4000-FILE-CLOSE paragraph to close the output files and terminates the program using GOBACK. The paragraph uses PAUTBPCB, PASFLPCB and PADFLPCB from the linkage section.

### 1000-INITIALIZE
> [Source: 1000-INITIALIZE.cbl.md](DBUNLDGS.CBL.d/1000-INITIALIZE.cbl.md)
This paragraph initializes the program environment. It accepts the current date and day from the system and stores them in CURRENT-DATE and CURRENT-YYDDD, respectively. It then displays a series of messages to the console indicating the start of the program and the current date. The commented-out code suggests that it was intended to accept parameters from SYSIN and open output files OPFILE1 and OPFILE2, with error handling for file open failures. However, these file operations are currently commented out. The paragraph consumes system date and day, and displays messages to the console. It does not directly produce any output data, but sets up the environment for subsequent processing. It does not call any other paragraphs.

## Control Flow

```mermaid
flowchart TD
    %% Title: DBUNLDGS.CBL
    1000_EXIT["1000-EXIT"]
    1000_INITIALIZE["1000-INITIALIZE"]
    2000_EXIT["2000-EXIT"]
    2000_FIND_NEXT_AUTH_SUMMARY["2000-FIND-NEXT-AUTH-SUMMARY"]
    3000_FIND_NEXT_AUTH_DTL["3000-FIND-NEXT-AUTH-DTL"]
    3100_INSERT_PARENT_SEG_GSAM["3100-INSERT-PARENT-SEG-GSAM"]
    9999_ABEND["9999-ABEND"]
    CBLTDLI__ext(["CBLTDLI"])
    3000_EXIT["3000-EXIT"]
    3200_INSERT_CHILD_SEG_GSAM["3200-INSERT-CHILD-SEG-GSAM"]
    3100_EXIT["3100-EXIT"]
    3200_EXIT["3200-EXIT"]
    4000_EXIT["4000-EXIT"]
    4000_FILE_CLOSE["4000-FILE-CLOSE"]
    9999_EXIT["9999-EXIT"]
    MAIN_PARA["MAIN-PARA"]
    2000_FIND_NEXT_AUTH_SUMMARY --> 3000_FIND_NEXT_AUTH_DTL
    2000_FIND_NEXT_AUTH_SUMMARY --> 3100_INSERT_PARENT_SEG_GSAM
    2000_FIND_NEXT_AUTH_SUMMARY --> 9999_ABEND
    2000_FIND_NEXT_AUTH_SUMMARY -.->|calls| CBLTDLI__ext
    3000_FIND_NEXT_AUTH_DTL --> 3200_INSERT_CHILD_SEG_GSAM
    3000_FIND_NEXT_AUTH_DTL --> 9999_ABEND
    3000_FIND_NEXT_AUTH_DTL -.->|calls| CBLTDLI__ext
    3100_INSERT_PARENT_SEG_GSAM --> 9999_ABEND
    3100_INSERT_PARENT_SEG_GSAM -.->|calls| CBLTDLI__ext
    3200_INSERT_CHILD_SEG_GSAM --> 9999_ABEND
    3200_INSERT_CHILD_SEG_GSAM -.->|calls| CBLTDLI__ext
    MAIN_PARA --> 1000_INITIALIZE
    MAIN_PARA --> 2000_FIND_NEXT_AUTH_SUMMARY
    MAIN_PARA --> 4000_FILE_CLOSE
```

## Open Questions

- ? What is the purpose of the IMSFUNCS copybook?
  - Context: The code includes the IMSFUNCS copybook, but its contents and usage are not evident in the provided snippet.
- ? What are the exact record layouts for OPFILE1 and OPFILE2?
  - Context: The file definitions for OPFILE1 and OPFILE2 are commented out, so their exact structure and content are unclear.
- ? What is the purpose of the 2000-FIND-NEXT-AUTH-SUMMARY and 4000-FILE-CLOSE paragraphs?
  - Context: The code for these paragraphs is not provided in the snippet.
- ? What are the contents of the PASFLPCB and PADFLPCB copybooks?
  - Context: The copybooks PASFLPCB and PADFLPCB are included, but their contents are not shown.

## Sequence Diagram

```mermaid
sequenceDiagram
    participant MAIN_PARA as MAIN-PARA
    participant 1000_INITIALIZE as 1000-INITIALIZE
    participant 2000_FIND_NEXT_AUTH_SUMMARY as 2000-FIND-NEXT-AUTH-SUMMARY
    participant 4000_FILE_CLOSE as 4000-FILE-CLOSE
    MAIN_PARA->>1000_INITIALIZE: performs
    1000_INITIALIZE-->>MAIN_PARA: CURRENT-DATE / CURRENT-YYDDD
    MAIN_PARA->>2000_FIND_NEXT_AUTH_SUMMARY: performs
    2000_FIND_NEXT_AUTH_SUMMARY-->>MAIN_PARA: WS-END-OF-ROOT-SEG / WS-NO-SUMRY-READ / WS-AUTH-SMRY-PROC-CNT / ...
    MAIN_PARA->>4000_FILE_CLOSE: performs
```
