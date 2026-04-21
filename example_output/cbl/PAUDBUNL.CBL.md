# PAUDBUNL

**File**: `cbl/PAUDBUNL.CBL`
**Type**: FileType.COBOL
**Analyzed**: 2026-04-21 13:41:27.884101

## Purpose

The COBOL program PAUDBUNL extracts authorization summary and detail records from an IMS database and writes them to two sequential output files. It reads authorization summary records and then retrieves associated detail records, writing each to a separate output file.

**Business Context**: This program likely supports auditing or reporting on authorization activities within a system, by extracting data from an IMS database into sequential files for further analysis.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUTBPCB | IOType.IMS_SEGMENT | IMS PCB used to access the authorization database. Contains status codes and segment information. |
| PENDING-AUTH-SUMMARY | IOType.IMS_SEGMENT | Authorization summary record read from the IMS database. |
| PENDING-AUTH-DETAILS | IOType.IMS_SEGMENT | Authorization detail record read from the IMS database. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| OPFILE1 | IOType.FILE_SEQUENTIAL | Sequential file containing authorization summary records extracted from the IMS database. |
| OPFILE2 | IOType.FILE_SEQUENTIAL | Sequential file containing authorization detail records extracted from the IMS database. |

## Called Programs

| Program | Call Type | Purpose |
|---------|-----------|---------|
| CBLTDLI | CallType.STATIC_CALL | Interface with IMS database to retrieve segments. |
| CBLTDLI | CallType.STATIC_CALL | Interface with IMS database to retrieve segments. |
| 9999-ABEND | CallType.STATIC_CALL | Abend the program due to an error condition. |
| 9999-ABEND | CallType.STATIC_CALL | Abend the program due to an error condition. |
| 9999-ABEND | CallType.STATIC_CALL | Abend the program due to an error condition. |
| 9999-ABEND | CallType.STATIC_CALL | Abend the program due to an error condition. |

## Business Rules

- **BR001**: Only write authorization summary records to OPFILE1 if the PA-ACCT-ID is numeric.

## Paragraphs/Procedures

### PAUDBUNL
This is the program's main paragraph. It serves as the entry point when called via 'DLITCBL' using the PAUTBPCB.  It essentially acts as a wrapper, immediately transferring control to the MAIN-PARA paragraph. No direct data manipulation or logic is present within this paragraph; its sole function is to initiate the program's execution flow by calling MAIN-PARA. This paragraph does not handle any specific input or output, nor does it implement any business rules or error handling. It simply ensures that the program begins its execution at the designated entry point.

### MAIN-PARA
> [Source: MAIN-PARA.cbl.md](PAUDBUNL.CBL.d/MAIN-PARA.cbl.md)

```
MAIN-PARA  (5 statements, depth=1)
PARAGRAPH
├── UNKNOWN: ENTRY 'DLITCBL'                 USING PAUTBPCB
├── PERFORM_THRU: PERFORM 1000-INITIALIZE                THRU 1000-EXIT
├── PERFORM_THRU: PERFORM 2000-FIND-NEXT-AUTH-SUMMARY    THRU 2000-EXIT
UNTIL   WS-END-OF-ROOT-SEG = 'Y'
├── PERFORM_THRU: PERFORM 4000-FILE-CLOSE THRU 4000-EXIT
└── GOBACK: GOBACK
```
This paragraph serves as the main control flow for the program. It first performs 1000-INITIALIZE to set up the program environment, including opening output files. Then, it enters a loop, repeatedly performing 2000-FIND-NEXT-AUTH-SUMMARY to read authorization summary records from the IMS database. The loop continues until WS-END-OF-ROOT-SEG is set to 'Y', indicating the end of the root segment. After processing all summary records, it performs 4000-FILE-CLOSE to close the output files. This paragraph orchestrates the overall process of extracting authorization data from IMS and writing it to sequential files. It consumes the PAUTBPCB from the linkage section. It calls 1000-INITIALIZE, 2000-FIND-NEXT-AUTH-SUMMARY, and 4000-FILE-CLOSE to perform the necessary steps.

### 1000-INITIALIZE
> [Source: 1000-INITIALIZE.cbl.md](PAUDBUNL.CBL.d/1000-INITIALIZE.cbl.md)

```
1000-INITIALIZE  (18 statements, depth=3)
PARAGRAPH
├── ACCEPT: ACCEPT CURRENT-DATE     FROM DATE
├── ACCEPT: ACCEPT CURRENT-YYDDD    FROM DAY
├── DISPLAY: DISPLAY 'STARTING PROGRAM PAUDBUNL::'
├── DISPLAY: DISPLAY '*-------------------------------------*'
├── DISPLAY: DISPLAY 'TODAYS DATE            :' CURRENT-DATE
├── DISPLAY: DISPLAY ' '
├── OPEN: OPEN OUTPUT OPFILE1
├── IF: IF WS-OUTFL1-STATUS =  SPACES OR '00'
│   ├── CONTINUE: CONTINUE
│   └── ELSE: ELSE
│       ├── DISPLAY: DISPLAY 'ERROR IN OPENING OPFILE1:' WS-OUTFL1-STATUS
│       └── PERFORM: PERFORM 9999-ABEND
├── OPEN: OPEN OUTPUT OPFILE2
└── IF: IF WS-OUTFL2-STATUS =  SPACES OR '00'
    ├── CONTINUE: CONTINUE
    └── ELSE: ELSE
        ├── DISPLAY: DISPLAY 'ERROR IN OPENING OPFILE2:' WS-OUTFL2-STATUS
        └── PERFORM: PERFORM 9999-ABEND
```
This paragraph initializes the program environment. It accepts the current date and day from the system. It then opens OPFILE1 and OPFILE2 for output. If either OPEN fails (WS-OUTFL1-STATUS or WS-OUTFL2-STATUS is not spaces or '00'), it displays an error message and performs 9999-ABEND to terminate the program. This paragraph ensures that the output files are available for writing. It consumes system date and day. It produces opened output files OPFILE1 and OPFILE2. It calls 9999-ABEND if file opening fails. The paragraph checks the file status after each OPEN statement and abends if the status is not successful.

### 1000-EXIT
> [Source: 1000-EXIT.cbl.md](PAUDBUNL.CBL.d/1000-EXIT.cbl.md)

```
1000-EXIT  (1 statements, depth=1)
PARAGRAPH
└── EXIT: EXIT
```
This paragraph serves as the exit point for the 1000-INITIALIZE paragraph. It simply contains the EXIT statement, allowing control to return to the calling paragraph (MAIN-PARA). It does not perform any data manipulation, error handling, or call any other paragraphs. Its sole purpose is to provide a defined exit point for the initialization process.

### 2000-FIND-NEXT-AUTH-SUMMARY
> [Source: 2000-FIND-NEXT-AUTH-SUMMARY.cbl.md](PAUDBUNL.CBL.d/2000-FIND-NEXT-AUTH-SUMMARY.cbl.md)

```
2000-FIND-NEXT-AUTH-SUMMARY  (20 statements, depth=3)
PARAGRAPH
├── INITIALIZE: INITIALIZE PAUT-PCB-STATUS
├── CALL: CALL 'CBLTDLI'            USING  FUNC-GN
PAUTBPCB
PENDING-AUTH-SUMMARY
ROOT-UNQUAL-SSA
├── IF: IF PAUT-PCB-STATUS = SPACES
│   ├── ADD: ADD 1                 TO WS-NO-SUMRY-READ
│   ├── ADD: ADD 1                 TO WS-AUTH-SMRY-PROC-CNT
│   ├── MOVE: MOVE PENDING-AUTH-SUMMARY TO OPFIL1-REC
│   ├── INITIALIZE: INITIALIZE ROOT-SEG-KEY
│   ├── INITIALIZE: INITIALIZE CHILD-SEG-REC
│   ├── MOVE: MOVE PA-ACCT-ID           TO ROOT-SEG-KEY
│   └── IF: IF PA-ACCT-ID IS NUMERIC
│       ├── WRITE: WRITE OPFIL1-REC
│       ├── INITIALIZE: INITIALIZE WS-END-OF-CHILD-SEG
│       └── PERFORM_THRU: PERFORM 3000-FIND-NEXT-AUTH-DTL THRU 3000-EXIT
UNTIL  WS-END-OF-CHILD-SEG='Y'
├── IF: IF PAUT-PCB-STATUS = 'GB'
│   ├── SET: SET END-OF-AUTHDB     TO TRUE
│   └── MOVE: MOVE 'Y' TO WS-END-OF-ROOT-SEG
└── IF: IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'GB'
    ├── DISPLAY: DISPLAY 'AUTH SUM  GN FAILED  :' PAUT-PCB-STATUS
    ├── DISPLAY: DISPLAY 'KEY FEEDBACK AREA    :' PAUT-KEYFB
    └── PERFORM: PERFORM 9999-ABEND
```
This paragraph retrieves the next authorization summary record from the IMS database. It initializes PAUT-PCB-STATUS and then calls CBLTDLI with FUNC-GN to get the next root segment (authorization summary). If the call is successful (PAUT-PCB-STATUS is spaces), it increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves the PENDING-AUTH-SUMMARY to OPFIL1-REC, initializes ROOT-SEG-KEY and CHILD-SEG-REC, moves PA-ACCT-ID to ROOT-SEG-KEY, and writes OPFIL1-REC to OPFILE1 if PA-ACCT-ID is numeric. Then, it performs 3000-FIND-NEXT-AUTH-DTL to retrieve the associated detail records. If PAUT-PCB-STATUS is 'GB', it sets END-OF-AUTHDB to TRUE and WS-END-OF-ROOT-SEG to 'Y', indicating the end of the database. If the call fails (PAUT-PCB-STATUS is not spaces or 'GB'), it displays an error message and performs 9999-ABEND. This paragraph is the core logic for reading summary records and initiating the retrieval of detail records. It consumes PAUTBPCB, PENDING-AUTH-SUMMARY, and ROOT-UNQUAL-SSA. It produces OPFIL1-REC and potentially calls 3000-FIND-NEXT-AUTH-DTL. It handles IMS call status and abends if the call fails.

### 2000-EXIT
> [Source: 2000-EXIT.cbl.md](PAUDBUNL.CBL.d/2000-EXIT.cbl.md)

```
2000-EXIT  (1 statements, depth=1)
PARAGRAPH
└── EXIT: EXIT
```
This paragraph serves as the exit point for the 2000-FIND-NEXT-AUTH-SUMMARY paragraph. It simply contains the EXIT statement, allowing control to return to the calling paragraph (MAIN-PARA). It does not perform any data manipulation, error handling, or call any other paragraphs. Its sole purpose is to provide a defined exit point for the summary record retrieval process.

### 3000-FIND-NEXT-AUTH-DTL
> [Source: 3000-FIND-NEXT-AUTH-DTL.cbl.md](PAUDBUNL.CBL.d/3000-FIND-NEXT-AUTH-DTL.cbl.md)

```
3000-FIND-NEXT-AUTH-DTL  (15 statements, depth=2)
PARAGRAPH
├── CALL: CALL 'CBLTDLI'            USING  FUNC-GNP
PAUTBPCB
PENDING-AUTH-DETAILS
CHILD-UNQUAL-SSA
├── IF: IF PAUT-PCB-STATUS = SPACES
│   ├── SET: SET MORE-AUTHS       TO TRUE
│   ├── ADD: ADD 1                 TO WS-NO-SUMRY-READ
│   ├── ADD: ADD 1                 TO WS-AUTH-SMRY-PROC-CNT
│   ├── MOVE: MOVE PENDING-AUTH-DETAILS TO CHILD-SEG-REC
│   └── WRITE: WRITE OPFIL2-REC
├── IF: IF PAUT-PCB-STATUS = 'GE'
│   ├── MOVE: MOVE 'Y' TO WS-END-OF-CHILD-SEG
│   └── DISPLAY: DISPLAY 'CHILD SEG FLAG GE : '
WS-END-OF-CHILD-SEG
├── IF: IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'GE'
│   ├── DISPLAY: DISPLAY 'GNP CALL FAILED  :' PAUT-PCB-STATUS
│   ├── DISPLAY: DISPLAY 'KFB AREA IN CHILD:' PAUT-KEYFB
│   └── PERFORM: PERFORM 9999-ABEND
└── INITIALIZE: INITIALIZE PAUT-PCB-STATUS
```
This paragraph retrieves the next authorization detail record from the IMS database. It calls CBLTDLI with FUNC-GNP to get the next child segment (authorization detail). If the call is successful (PAUT-PCB-STATUS is spaces), it sets MORE-AUTHS to TRUE, increments WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT, moves PENDING-AUTH-DETAILS to CHILD-SEG-REC, and writes CHILD-SEG-REC to OPFILE2. If PAUT-PCB-STATUS is 'GE', it moves 'Y' to WS-END-OF-CHILD-SEG, indicating the end of the child segments. If the call fails (PAUT-PCB-STATUS is not spaces or 'GE'), it displays an error message and performs 9999-ABEND. Finally, it initializes PAUT-PCB-STATUS. This paragraph is responsible for reading detail records associated with a summary record. It consumes PAUTBPCB, PENDING-AUTH-DETAILS, and CHILD-UNQUAL-SSA. It produces CHILD-SEG-REC. It handles IMS call status and abends if the call fails.

### 3000-EXIT
> [Source: 3000-EXIT.cbl.md](PAUDBUNL.CBL.d/3000-EXIT.cbl.md)

```
3000-EXIT  (1 statements, depth=1)
PARAGRAPH
└── EXIT: EXIT
```
This paragraph serves as the exit point for the 3000-FIND-NEXT-AUTH-DTL paragraph. It simply contains the EXIT statement, allowing control to return to the calling paragraph (2000-FIND-NEXT-AUTH-SUMMARY). It does not perform any data manipulation, error handling, or call any other paragraphs. Its sole purpose is to provide a defined exit point for the detail record retrieval process.

### 4000-FILE-CLOSE
> [Source: 4000-FILE-CLOSE.cbl.md](PAUDBUNL.CBL.d/4000-FILE-CLOSE.cbl.md)

```
4000-FILE-CLOSE  (11 statements, depth=3)
PARAGRAPH
├── DISPLAY: DISPLAY 'CLOSING THE FILE'
├── CLOSE: CLOSE OPFILE1
├── IF: IF WS-OUTFL1-STATUS =  SPACES OR '00'
│   ├── CONTINUE: CONTINUE
│   └── ELSE: ELSE
│       └── DISPLAY: DISPLAY 'ERROR IN CLOSING 1ST FILE:'WS-OUTFL1-STATUS
├── CLOSE: CLOSE OPFILE2
└── IF: IF WS-OUTFL2-STATUS =  SPACES OR '00'
    ├── CONTINUE: CONTINUE
    └── ELSE: ELSE
        └── DISPLAY: DISPLAY 'ERROR IN CLOSING 2ND FILE:'WS-OUTFL2-STATUS
```
This paragraph closes the output files OPFILE1 and OPFILE2. It displays a message indicating that the files are being closed. It then closes each file and checks the file status (WS-OUTFL1-STATUS and WS-OUTFL2-STATUS). If the close operation fails for either file (status is not spaces or '00'), it displays an error message. This paragraph ensures that the output files are properly closed after processing is complete. It consumes WS-OUTFL1-STATUS and WS-OUTFL2-STATUS. It produces closed output files OPFILE1 and OPFILE2. It does not call any other paragraphs. The paragraph checks the file status after each CLOSE statement and displays an error if the status is not successful, but does not abend.

### 4000-EXIT
> [Source: 4000-EXIT.cbl.md](PAUDBUNL.CBL.d/4000-EXIT.cbl.md)

```
4000-EXIT  (1 statements, depth=1)
PARAGRAPH
└── EXIT: EXIT
```
This paragraph serves as the exit point for the 4000-FILE-CLOSE paragraph. It simply contains the EXIT statement, allowing control to return to the calling paragraph (MAIN-PARA). It does not perform any data manipulation, error handling, or call any other paragraphs. Its sole purpose is to provide a defined exit point for the file closing process.

### 9999-ABEND
> [Source: 9999-ABEND.cbl.md](PAUDBUNL.CBL.d/9999-ABEND.cbl.md)

```
9999-ABEND  (3 statements, depth=1)
PARAGRAPH
├── DISPLAY: DISPLAY 'IMSUNLOD ABENDING ...'
├── MOVE: MOVE 16 TO RETURN-CODE
└── GOBACK: GOBACK
```
This paragraph handles the abnormal termination of the program. Its primary purpose is to display an error message ('IMSUNLOD ABENDING ...') to the console, indicating that the program has encountered a critical error and is terminating prematurely. The paragraph then sets the RETURN-CODE to 16, which signals a severe error condition to the calling environment. Finally, the paragraph uses the GOBACK statement to terminate the program execution and return control to the operating system. This paragraph does not consume any specific input files or data, nor does it produce any output files other than the display message. It is likely called from other paragraphs within the program when an unrecoverable error is detected.

### 9999-EXIT
> [Source: 9999-EXIT.cbl.md](PAUDBUNL.CBL.d/9999-EXIT.cbl.md)

```
9999-EXIT  (1 statements, depth=1)
PARAGRAPH
└── EXIT: EXIT
```
This paragraph serves as a standard exit point for the program. Its primary purpose is to provide a clean and controlled way to terminate the program's execution. It simply contains the EXIT statement, which returns control to the calling paragraph or program. This paragraph does not consume any input data or produce any output data. It is likely called from other paragraphs within the program when normal processing is complete and the program needs to terminate gracefully. The paragraph ensures that the program exits in a predictable manner, allowing for proper resource cleanup and return code setting in other parts of the program.

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

- ? What is the structure of the IMS database segments (PENDING-AUTH-SUMMARY, PENDING-AUTH-DETAILS)?
  - Context: The copybooks for these segments are not provided, so their exact structure and fields are unknown.
- ? What is the purpose of WS-NO-SUMRY-READ and WS-AUTH-SMRY-PROC-CNT?
  - Context: These variables are incremented in both 2000-FIND-NEXT-AUTH-SUMMARY and 3000-FIND-NEXT-AUTH-DTL, but their usage is not clear.
- ? What is the structure of ROOT-UNQUAL-SSA and CHILD-UNQUAL-SSA?
  - Context: These are used in the IMS calls, but their structure is not defined in the provided code.
