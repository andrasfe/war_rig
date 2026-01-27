# PAUDBLOD

**File**: `cbl/PAUDBLOD.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-27 23:04:29.893971

## Purpose

The COBOL program PAUDBLOD reads root and child segment data from two sequential input files (INFILE1 and INFILE2) and inserts them into an IMS database. It reads pending authorization summary records from INFILE1 and pending authorization detail records from INFILE2, then inserts them as root and child segments respectively into the IMS database.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| INFILE1 | IOType.FILE_SEQUENTIAL | Contains pending authorization summary records to be inserted as root segments into the IMS database. |
| INFILE2 | IOType.FILE_SEQUENTIAL | Contains pending authorization detail records to be inserted as child segments into the IMS database.  The ROOT-SEG-KEY field contains the key of the parent root segment. |
| PAUTBPCB | IOType.PARAMETER | IMS Program Communication Block (PCB) mask, used for IMS calls. |
| IO-PCB-MASK | IOType.PARAMETER | IO PCB Mask |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| IMS Database | IOType.IMS_SEGMENT | The IMS database is updated with pending authorization summary (root) and detail (child) segments. |
| RETURN-CODE | IOType.RETURN_CODE | Return code set to 16 if the program abends. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| CBLTDLI | CallType.STATIC_CALL | Interface with IMS DL/I to perform database operations (ISRT, GU). |
| CBLTDLI | CallType.STATIC_CALL | Interface with IMS DL/I to perform database operations (GU). |
| CBLTDLI | CallType.STATIC_CALL | Interface with IMS DL/I to perform database operations (ISRT). |

## Paragraphs/Procedures

### MAIN-PARA
This is the main control paragraph of the program. It orchestrates the overall process of reading root and child segment data from input files and inserting them into an IMS database. It first calls 1000-INITIALIZE to open the input files and perform initial setup. Then, it enters a loop that reads root segments from INFILE1 using 2000-READ-ROOT-SEG-FILE until the end of the file is reached.  Subsequently, it enters another loop that reads child segments from INFILE2 using 3000-READ-CHILD-SEG-FILE until the end of that file. Finally, it calls 4000-FILE-CLOSE to close the input files before terminating the program. The program uses flags END-ROOT-SEG-FILE and END-CHILD-SEG-FILE to control the read loops. The program abends if there are errors opening the input files, or during root segment insertion.

### 1000-INITIALIZE
This paragraph initializes the program by accepting the current date and opening the input files INFILE1 and INFILE2. It accepts the current date and current year-day from the system. It then attempts to open INFILE1 and INFILE2 in INPUT mode. The file status codes WS-INFIL1-STATUS and WS-INFIL2-STATUS are checked after each OPEN. If either file fails to open (status is not SPACES or '00'), an error message is displayed, and the program abends by calling 9999-ABEND. If the files open successfully, the program continues to the next step. No data is directly read or written in this paragraph, it only prepares the environment for subsequent processing.

### 2000-READ-ROOT-SEG-FILE
This paragraph reads records from INFILE1 and attempts to insert them as root segments into the IMS database. It reads a record from INFILE1 into INFIL1-REC. If the read is successful (WS-INFIL1-STATUS is SPACES or '00'), it moves the contents of INFIL1-REC to PENDING-AUTH-SUMMARY and then calls 2100-INSERT-ROOT-SEG to insert the segment into the IMS database. If the end of the file is reached (WS-INFIL1-STATUS is '10'), it sets the END-ROOT-SEG-FILE flag to 'Y'. If any other error occurs during the read, an error message is displayed. The paragraph uses the END-ROOT-SEG-FILE flag to signal the end of the input file to the calling paragraph (MAIN-PARA).

### 2100-INSERT-ROOT-SEG
This paragraph inserts a root segment into the IMS database using the CBLTDLI call. It calls the CBLTDLI routine with the FUNC-ISRT function code to insert the PENDING-AUTH-SUMMARY segment. The PAUTBPCB and ROOT-UNQUAL-SSA are also passed to the CBLTDLI call. After the call, the PAUT-PCB-STATUS is checked. If the status is SPACES, it indicates a successful insertion. If the status is 'II', it indicates that the root segment already exists in the database. If the status is anything else, it indicates an error, and the program abends by calling 9999-ABEND. The paragraph displays messages indicating the success or failure of the insertion based on the PCB status.

### 3000-READ-CHILD-SEG-FILE
This paragraph reads records from INFILE2 and attempts to insert them as child segments into the IMS database. It reads a record from INFILE2. If the read is successful (WS-INFIL2-STATUS is SPACES or '00') and ROOT-SEG-KEY is numeric, it moves the ROOT-SEG-KEY to QUAL-SSA-KEY-VALUE and the CHILD-SEG-REC to PENDING-AUTH-DETAILS. Then, it calls 3100-INSERT-CHILD-SEG to insert the child segment into the IMS database. If the end of the file is reached (WS-INFIL2-STATUS is '10'), it sets the END-CHILD-SEG-FILE flag to 'Y'. If any other error occurs during the read, an error message is displayed. The paragraph uses the END-CHILD-SEG-FILE flag to signal the end of the input file to the calling paragraph (MAIN-PARA).

### 3100-INSERT-CHILD-SEG
This paragraph retrieves the root segment and then inserts a child segment into the IMS database. It first initializes PAUT-PCB-STATUS. It then calls CBLTDLI with FUNC-GU to retrieve the root segment using the qualified SSA (ROOT-QUAL-SSA). If the GU call is successful (PAUT-PCB-STATUS is SPACES), it calls 3200-INSERT-IMS-CALL to insert the child segment. If the GU call fails (PAUT-PCB-STATUS is not SPACES), an error message is displayed, and the program abends by calling 9999-ABEND. The paragraph relies on the ROOT-QUAL-SSA to specify which root segment the child segment should be associated with. The success or failure of the GU call determines whether the child segment insertion is attempted.

### 3200-INSERT-IMS-CALL
This paragraph inserts a child segment into the IMS database using the CBLTDLI call. It calls CBLTDLI with FUNC-ISRT to insert the PENDING-AUTH-DETAILS segment using the CHILD-UNQUAL-SSA. After the call, the PAUT-PCB-STATUS is checked. If the status is SPACES, it indicates a successful insertion. If the status is 'II', it indicates that the child segment already exists in the database. If the status is anything else, it indicates an error, and the program abends by calling 9999-ABEND. The paragraph displays messages indicating the success or failure of the insertion based on the PCB status.

### 4000-FILE-CLOSE
This paragraph closes the input files INFILE1 and INFILE2. It attempts to close each file and checks the file status after each close operation. If the file status is not SPACES or '00', an error message is displayed, but the program does NOT abend. The paragraph ensures that all files are properly closed before the program terminates. The error handling in this paragraph only displays an error message and continues, unlike the OPEN and INSERT paragraphs which abend on error.

### 9999-ABEND
This paragraph handles program termination due to an error condition. It displays a message indicating that the IMS load is abending and sets the RETURN-CODE to 16. The program then terminates using the GOBACK statement. This paragraph is called from various points in the program where critical errors are detected, such as file open failures or IMS database insertion errors. The setting of the RETURN-CODE allows the calling environment to recognize that the program terminated abnormally.

## Control Flow

```mermaid
flowchart TD
    %% Title: PAUDBLOD.CBL
    1000_EXIT["1000-EXIT"]
    1000_INITIALIZE["1000-INITIALIZE"]
    9999_ABEND["9999-ABEND"]
    2000_EXIT["2000-EXIT"]
    2000_READ_ROOT_SEG_FILE["2000-READ-ROOT-SEG-FILE"]
    2100_INSERT_ROOT_SEG["2100-INSERT-ROOT-SEG"]
    2100_EXIT["2100-EXIT"]
    CBLTDLI__ext(["CBLTDLI"])
    3000_EXIT["3000-EXIT"]
    3000_READ_CHILD_SEG_FILE["3000-READ-CHILD-SEG-FILE"]
    3100_INSERT_CHILD_SEG["3100-INSERT-CHILD-SEG"]
    3100_EXIT["3100-EXIT"]
    3200_INSERT_IMS_CALL["3200-INSERT-IMS-CALL"]
    3200_EXIT["3200-EXIT"]
    4000_EXIT["4000-EXIT"]
    4000_FILE_CLOSE["4000-FILE-CLOSE"]
    9999_EXIT["9999-EXIT"]
    MAIN_PARA["MAIN-PARA"]
    1000_INITIALIZE --> 9999_ABEND
    2000_READ_ROOT_SEG_FILE --> 2100_INSERT_ROOT_SEG
    2100_INSERT_ROOT_SEG --> 9999_ABEND
    2100_INSERT_ROOT_SEG -.->|calls| CBLTDLI__ext
    3000_READ_CHILD_SEG_FILE --> 3100_INSERT_CHILD_SEG
    3100_INSERT_CHILD_SEG --> 3200_INSERT_IMS_CALL
    3100_INSERT_CHILD_SEG --> 9999_ABEND
    3100_INSERT_CHILD_SEG -.->|calls| CBLTDLI__ext
    3200_INSERT_IMS_CALL --> 9999_ABEND
    3200_INSERT_IMS_CALL -.->|calls| CBLTDLI__ext
    MAIN_PARA --> 1000_INITIALIZE
    MAIN_PARA --> 2000_READ_ROOT_SEG_FILE
    MAIN_PARA --> 3000_READ_CHILD_SEG_FILE
    MAIN_PARA --> 4000_FILE_CLOSE
```

## Open Questions

- ? What is the purpose of the COPY IMSFUNCS?
  - Context: The code includes COPY IMSFUNCS, but the contents of this copybook are not available. Without knowing the contents, it is difficult to determine the exact function codes used in the CBLTDLI calls.
- ? What is the structure of the PAUTBPCB copybook?
  - Context: The code uses PAUTBPCB in the linkage section and in the CBLTDLI calls, but the structure of this copybook is not available. Without knowing the structure, it is difficult to fully understand the IMS calls.
- ? What is the exact structure of the PENDING-AUTH-SUMMARY and PENDING-AUTH-DETAILS segments?
  - Context: The code uses COPY CIPAUSMY and COPY CIPAUDTY to define these segments, but the contents of these copybooks are not available. Without knowing the exact structure, it is difficult to fully understand the data being inserted into the IMS database.
