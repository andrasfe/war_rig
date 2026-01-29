# DBUNLDGS - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** DBUNLDGS
- **File Name:** DBUNLDGS.CBL
- **File Type:** COBOL
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:54:18.058346

## Purpose

**Summary:** This COBOL program, named DBUNLDGS, extracts data related to pending authorizations from an IMS database. It retrieves pending authorization summary and detail segments and writes them to GSAM files. The program uses IMS calls to navigate the database and GSAM calls to write the extracted data.

**Business Context:** None
**Program Type:** BATCH

## Inputs

### PAUTBPCB

- **Type:** PARAMETER
- **Description:** Pending Authorization Summary PCB mask
- **Copybook:** PAUTBPCB

### PASFLPCB

- **Type:** PARAMETER
- **Description:** Pending Authorization Summary GSAM PCB mask
- **Copybook:** PASFLPCB

### PADFLPCB

- **Type:** PARAMETER
- **Description:** Pending Authorization Detail GSAM PCB mask
- **Copybook:** PADFLPCB

## Outputs

### OPFILE1

- **Type:** FILE_SEQUENTIAL
- **Description:** Output file containing pending authorization summary records.

### OPFILE2

- **Type:** FILE_SEQUENTIAL
- **Description:** Output file containing pending authorization detail records.

## Business Rules

### BR001

**Description:** The program processes pending authorization summary records only if the account ID (PA-ACCT-ID) is numeric.

**Logic:** The program checks if PA-ACCT-ID is numeric before writing the summary record to OPFILE1 and processing detail records.

**Conditions:**
- `IF PA-ACCT-ID IS NUMERIC`

### BR002

**Description:** The program retrieves pending authorization detail records until the end of the child segment is reached.

**Logic:** The program loops through the 3000-FIND-NEXT-AUTH-DTL paragraph until WS-END-OF-CHILD-SEG is set to 'Y'.

**Conditions:**
- `UNTIL WS-END-OF-CHILD-SEG='Y'`

## Paragraphs

### MAIN-PARA

This is the main entry point of the program. It orchestrates the overall process of extracting pending authorization data from the IMS database and writing it to output files. It first calls 1000-INITIALIZE to perform initial setup tasks such as accepting the current date. Then, it enters a loop, repeatedly calling 2000-FIND-NEXT-AUTH-SUMMARY to retrieve and process pending authorization summary records until the end of the database is reached (WS-END-OF-ROOT-SEG = 'Y'). Finally, it calls 4000-FILE-CLOSE to close the output files before terminating the program. The paragraph uses the PAUTBPCB, PASFLPCB, and PADFLPCB PCBs passed in the linkage section to communicate with IMS.

**Calls:** 1000-INITIALIZE, 2000-FIND-NEXT-AUTH-SUMMARY, 4000-FILE-CLOSE

### 1000-INITIALIZE

This paragraph performs initialization tasks required before the main processing loop. It accepts the current date and day from the system. It then displays the program name and current date on the console. The commented-out code suggests that it was originally intended to open output files (OPFILE1 and OPFILE2) and check their status, but this functionality is currently disabled. If the file opening failed, the program would have abended. Currently, it does not read any input files or parameters, and it primarily focuses on displaying information. It does not call any other paragraphs besides the implicit fallthrough to 1000-EXIT.

### 2000-FIND-NEXT-AUTH-SUMMARY

This paragraph retrieves the next pending authorization summary record from the IMS database. It first initializes the PAUT-PCB-STATUS field. Then, it calls the CBLTDLI routine with the FUNC-GN (Get Next) function to retrieve the next summary segment using the PAUTBPCB PCB and the ROOT-UNQUAL-SSA. If the PCB status (PAUT-PCB-STATUS) is spaces, it indicates a successful read, and the program increments counters (WS-NO-SUMRY-READ, WS-AUTH-SMRY-PROC-CNT), moves the retrieved summary data to OPFIL1-REC, and then calls 3000-FIND-NEXT-AUTH-DTL to process the detail records associated with the summary record. If the PA-ACCT-ID is numeric, the program calls 3100-INSERT-PARENT-SEG-GSAM to write the parent segment to GSAM. If the PCB status is 'GB' (end of database), it sets the WS-END-OF-ROOT-SEG flag to 'Y' to terminate the main processing loop. If the PCB status indicates an error, the program displays an error message and abends.

**Calls:** 3000-FIND-NEXT-AUTH-DTL, 3100-INSERT-PARENT-SEG-GSAM

### 3000-FIND-NEXT-AUTH-DTL

This paragraph retrieves the next pending authorization detail record associated with the current summary record. It calls the CBLTDLI routine with the FUNC-GNP (Get Next within Parent) function to retrieve the next detail segment using the PAUTBPCB PCB and the CHILD-UNQUAL-SSA. If the PCB status (PAUT-PCB-STATUS) is spaces, it indicates a successful read, and the program increments counters (WS-NO-SUMRY-READ, WS-AUTH-SMRY-PROC-CNT), moves the retrieved detail data to CHILD-SEG-REC, and calls 3200-INSERT-CHILD-SEG-GSAM to write the child segment to GSAM. If the PCB status is 'GE' (end of parent), it sets the WS-END-OF-CHILD-SEG flag to 'Y' to indicate that there are no more detail records for the current summary record. If the PCB status indicates an error, the program displays an error message and abends. The paragraph initializes PAUT-PCB-STATUS before exiting.

**Calls:** 3200-INSERT-CHILD-SEG-GSAM

### 3100-INSERT-PARENT-SEG-GSAM

This paragraph inserts the parent segment (PENDING-AUTH-SUMMARY) into the GSAM file. It calls the CBLTDLI routine with the FUNC-ISRT (Insert) function, using the PASFLPCB. If the PASFL-PCB-STATUS is not equal to spaces, it indicates an error, and the program displays an error message and abends. This paragraph takes PENDING-AUTH-SUMMARY as input and writes to the GSAM file defined by PASFLPCB. It's called from 2000-FIND-NEXT-AUTH-SUMMARY after a successful read of the root segment from the IMS database.

### 3200-INSERT-CHILD-SEG-GSAM

This paragraph inserts the child segment (PENDING-AUTH-DETAILS) into the GSAM file. It calls the CBLTDLI routine with the FUNC-ISRT (Insert) function, using the PADFLPCB. If the PADFL-PCB-STATUS is not equal to spaces, it indicates an error, and the program displays an error message and abends. This paragraph takes PENDING-AUTH-DETAILS as input and writes to the GSAM file defined by PADFLPCB. It's called from 3000-FIND-NEXT-AUTH-DTL after a successful read of the child segment from the IMS database.

### 4000-FILE-CLOSE

This paragraph is responsible for closing the output files. It displays a message indicating that the files are being closed. The actual CLOSE statements are commented out, along with the associated error checking logic. Therefore, this paragraph currently does not perform any file closing operations. It is called by MAIN-PARA after all the pending authorization records have been processed. It does not receive any input data and does not produce any output other than the display message.

### 9999-ABEND

This paragraph handles abnormal termination of the program. It displays a message indicating that the program is abending. It sets the RETURN-CODE to 16 and then terminates the program using the GOBACK statement. This paragraph is called when a critical error occurs, such as a failed IMS call or a file I/O error. It does not receive any input data and does not produce any output other than the display message and the return code.

## Data Flow

### Reads From

- **IMS Database (PAUTBPCB, PENDING-AUTH-SUMMARY):** PA-ACCT-ID
- **IMS Database (PAUTBPCB, PENDING-AUTH-DETAILS):** 

### Writes To

- **OPFILE1:** PENDING-AUTH-SUMMARY
- **OPFILE2:** PENDING-AUTH-DETAILS

## Error Handling

- **PAUT-PCB-STATUS NOT EQUAL TO SPACES AND 'GB' (Error during summary retrieval):** DISPLAY error message and ABEND
- **PAUT-PCB-STATUS NOT EQUAL TO SPACES AND 'GE' (Error during detail retrieval):** DISPLAY error message and ABEND
- **PASFL-PCB-STATUS NOT EQUAL TO SPACES (Error during GSAM parent insert):** DISPLAY error message and ABEND
- **PADFL-PCB-STATUS NOT EQUAL TO SPACES (Error during GSAM child insert):** DISPLAY error message and ABEND

## Flow Diagram

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