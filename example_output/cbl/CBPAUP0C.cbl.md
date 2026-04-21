# CBPAUP0C

**File:** CBPAUP0C.cbl
**Type:** COBOL
**Status:** In Progress
**Iterations:** 1
**Analyzed:** 2026-04-21 13:42:30.516346

## Purpose

The CBPAUP0C program purges expired authorization records from the IMS PAUT database. It reads authorization summary and detail segments, determines if authorizations have expired based on a configurable expiry period, and deletes expired detail records. It also deletes summary records if all associated detail records have been deleted and takes checkpoints periodically.

**Business Context:** This program maintains the PAUT authorization database by removing old authorization records, ensuring efficient storage and retrieval of current authorization data.
**Program Type:** BATCH
**Citations:** Lines 5, 11, 18

## Inputs

### PAUTSUM0
- **Type:** IMS_SEGMENT
- **Description:** Pending Authorization Summary segment from the IMS PAUT database. Contains summary information about authorizations.
- **Copybook:** [PENDING-AUTH-SUMMARY](../copybooks/PENDING-AUTH-SUMMARY.cpy.md)
- **Lines:** 94

### PAUTDTL1
- **Type:** IMS_SEGMENT
- **Description:** Pending Authorization Detail segment from the IMS PAUT database. Contains detailed information about individual authorizations.
- **Copybook:** [PENDING-AUTH-DETAILS](../copybooks/PENDING-AUTH-DETAILS.cpy.md)
- **Lines:** 127

### SYSIN
- **Type:** PARAMETER
- **Description:** Parameter input from SYSIN containing expiry days, checkpoint frequency, and debug flag.
- **Copybook:** [PRM-INFO](../copybooks/PRM-INFO.cpy.md)
- **Lines:** 58, 61

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [9999-ABEND](./9999-ABEND.cbl.md) | STATIC_CALL | Abends the program when an error occurs during IMS segment read. | 110 |

## Business Rules

### BR001: Authorization records are considered expired if the difference between the current date and the authorization date is greater than or equal to the expiry days specified in the input parameters.
**Logic:** The program calculates the difference between the current date and the authorization date and compares it to the expiry days.
**Conditions:** WS-DAY-DIFF >= WS-EXPIRY-DAYS
**Lines:** 155, 157

### BR002: If an authorization is approved (PA-AUTH-RESP-CODE = '00'), the approved authorization count and amount in the summary record are decremented by the authorization amount.
**Logic:** Subtracts 1 from PA-APPROVED-AUTH-CNT and PA-APPROVED-AMT from PA-APPROVED-AUTH-AMT.
**Conditions:** PA-AUTH-RESP-CODE = '00'
**Lines:** 160, 161, 162

### BR003: If an authorization is declined (PA-AUTH-RESP-CODE not = '00'), the declined authorization count and amount in the summary record are decremented by the authorization amount.
**Logic:** Subtracts 1 from PA-DECLINED-AUTH-CNT and PA-TRANSACTION-AMT from PA-DECLINED-AUTH-AMT.
**Conditions:** PA-AUTH-RESP-CODE not = '00'
**Lines:** 163, 164, 165

## Copybooks Used

| Copybook | Location | Purpose | Line |
|----------|----------|---------|------|
| [PENDING-AUTH-SUMMARY](../copybooks/PENDING-AUTH-SUMMARY.cpy.md) | WORKING_STORAGE | Defines the structure of the PAUTSUM0 IMS segment. | 95 |
| [PENDING-AUTH-DETAILS](../copybooks/PENDING-AUTH-DETAILS.cpy.md) | WORKING_STORAGE | Defines the structure of the PAUTDTL1 IMS segment. | 128 |
| [PRM-INFO](../copybooks/PRM-INFO.cpy.md) | WORKING_STORAGE | Defines the structure of the input parameters from SYSIN. | 58 |

## Data Flow

### Reads From
- **PAUTSUM0**: PA-ACCT-ID, PA-APPROVED-AUTH-CNT, PA-APPROVED-AUTH-AMT, PA-DECLINED-AUTH-CNT, PA-DECLINED-AUTH-AMT
  (Lines: 94)
- **PAUTDTL1**: PA-AUTH-DATE-9C, PA-AUTH-RESP-CODE, PA-APPROVED-AMT, PA-TRANSACTION-AMT
  (Lines: 127)
- **SYSIN**: P-EXPIRY-DAYS, P-CHKP-FREQ, P-CHKP-DIS-FREQ, P-DEBUG-FLAG
  (Lines: 58)

### Transformations
- **PA-AUTH-DATE-9C** → **WS-AUTH-DATE**: Calculates the authorization date by subtracting PA-AUTH-DATE-9C from 99999.
  (Lines: 153)
- **CURRENT-YYDDD** → **WS-DAY-DIFF**: Calculates the difference between the current date (CURRENT-YYDDD) and the calculated authorization date (WS-AUTH-DATE).
  (Lines: 155)

## Key Paragraphs

### MAIN-PARA
**Purpose:** This paragraph is the main control loop of the CBPAUP0C program. It orchestrates the process of reading authorization summary and detail records from the IMS PAUT database, checking for expired authorizations, deleting expired detail records, and deleting summary records if all associated detail records are deleted. The paragraph first performs 1000-INITIALIZE to initialize variables and read input parameters. It then enters a loop that continues until either an error flag is set or the end of the authorization database is reached (ERR-FLG-ON OR END-OF-AUTHDB). Inside this loop, it reads authorization detail records using 3000-FIND-NEXT-AUTH-DTL until no more detail records are found (NO-MORE-AUTHS). For each detail record, it calls 4000-CHECK-IF-EXPIRED to determine if the authorization has expired. If an authorization is qualified for deletion (QUALIFIED-FOR-DELETE), 5000-DELETE-AUTH-DTL is performed. After processing all detail records for a summary record, it checks if both approved and declined authorization counts are less than or equal to zero. If so, it performs 6000-DELETE-AUTH-SUMMARY. Checkpoints are taken periodically using 9000-TAKE-CHECKPOINT based on the WS-AUTH-SMRY-PROC-CNT and P-CHKP-FREQ. Finally, the program displays summary statistics and terminates.
- Calls: 1000-INITIALIZE, 2000-FIND-NEXT-AUTH-SUMMARY, 3000-FIND-NEXT-AUTH-DTL, 4000-CHECK-IF-EXPIRED, 5000-DELETE-AUTH-DTL, 6000-DELETE-AUTH-SUMMARY, 9000-TAKE-CHECKPOINT
- Lines: 136-180

### 1000-INITIALIZE
**Purpose:** This paragraph initializes the program by accepting the current date and day, and reading input parameters from SYSIN. It displays the starting message, the received parameters, and today's date. It checks if the expiry days (P-EXPIRY-DAYS) is numeric. If it is, it moves the value to WS-EXPIRY-DAYS; otherwise, it defaults to 5 days. It checks if the checkpoint frequency (P-CHKP-FREQ) is spaces, 0, or LOW-VALUES. If so, it defaults to 5. It also checks if the checkpoint display frequency (P-CHKP-DIS-FREQ) is spaces, 0, or LOW-VALUES, defaulting to 10 if it is. Finally, it checks if the debug flag (P-DEBUG-FLAG) is not 'Y', and if so, sets it to 'N'. This paragraph ensures that the program has valid initial values for its key parameters and flags.
- Called by: MAIN-PARA
- Lines: 183-210

### 1000-EXIT
**Purpose:** This paragraph is the exit point for the 1000-INITIALIZE paragraph. It simply contains the EXIT statement, allowing control to return to the calling paragraph.
- Called by: MAIN-PARA
- Lines: 212-213

### 2000-FIND-NEXT-AUTH-SUMMARY
**Purpose:** This paragraph reads the next authorization summary record from the IMS PAUT database. It uses the EXEC DLI GN command to retrieve the next PAUTSUM0 segment. If the read is successful (DIBSTAT = '  '), it sets the NOT-END-OF-AUTHDB flag to TRUE, increments the summary read counter (WS-NO-SUMRY-READ), increments the authorization summary process counter (WS-AUTH-SMRY-PROC-CNT), and moves the account ID (PA-ACCT-ID) to WS-CURR-APP-ID. If the end of the database is reached (DIBSTAT = 'GB'), it sets the END-OF-AUTHDB flag to TRUE. If any other error occurs during the read, it displays an error message and abends the program by performing 9999-ABEND. This paragraph is responsible for sequentially accessing the authorization summary records.
- Called by: MAIN-PARA
- Calls: 9999-ABEND
- Lines: 216-242

### 2000-EXIT
**Purpose:** This paragraph is the exit point for the 2000-FIND-NEXT-AUTH-SUMMARY paragraph. It simply contains the EXIT statement, allowing control to return to the calling paragraph.
- Called by: MAIN-PARA
- Lines: 243-244

### 3000-FIND-NEXT-AUTH-DTL
**Purpose:** This paragraph reads the next authorization detail record from the IMS PAUT database. It uses the EXEC DLI GNP command to retrieve the next PAUTDTL1 segment. If the read is successful (DIBSTAT = '  '), it sets the MORE-AUTHS flag to TRUE and increments the detail read counter (WS-NO-DTL-READ). If the end of the segment is reached (DIBSTAT = 'GE' or 'GB'), it sets the NO-MORE-AUTHS flag to TRUE. If any other error occurs during the read, it displays an error message, the summary account ID, and abends the program by performing 9999-ABEND. This paragraph is responsible for sequentially accessing the authorization detail records associated with a summary record.
- Called by: MAIN-PARA
- Calls: 9999-ABEND
- Lines: 248-272

### 3000-EXIT
**Purpose:** This paragraph is the exit point for the 3000-FIND-NEXT-AUTH-DTL paragraph. It simply contains the EXIT statement, allowing control to return to the calling paragraph.
- Called by: MAIN-PARA
- Lines: 273-274

### 4000-CHECK-IF-EXPIRED
**Purpose:** This paragraph checks if an authorization detail record has expired. It calculates the authorization date (WS-AUTH-DATE) by subtracting the PA-AUTH-DATE-9C from 99999. It then calculates the difference (WS-DAY-DIFF) between the current date (CURRENT-YYDDD) and the calculated authorization date. If the difference is greater than or equal to the expiry days (WS-EXPIRY-DAYS), it sets the QUALIFIED-FOR-DELETE flag to TRUE. If the authorization response code (PA-AUTH-RESP-CODE) is '00' (approved), it decrements the approved authorization count (PA-APPROVED-AUTH-CNT) and the approved authorization amount (PA-APPROVED-AUTH-AMT) in the summary record. Otherwise (declined), it decrements the declined authorization count (PA-DECLINED-AUTH-CNT) and the declined authorization amount (PA-TRANSACTION-AMT) in the summary record. If the authorization has not expired, it sets the NOT-QUALIFIED-FOR-DELETE flag to TRUE. This paragraph implements the core business rule for determining authorization expiry.
- Called by: MAIN-PARA
- Lines: 277-298

### 4000-EXIT
**Purpose:** This paragraph is the exit point for the 4000-CHECK-IF-EXPIRED paragraph. It simply contains the EXIT statement, allowing control to return to the calling paragraph.
- Called by: MAIN-PARA
- Lines: 299-300

### 5000-DELETE-AUTH-DTL
**Purpose:** This paragraph deletes an authorization detail segment (PAUTDTL1) from the IMS database. It uses the PAUT-PCB-NUM to specify the PCB and PENDING-AUTH-DETAILS as the source of the segment to be deleted. Before deleting, it checks if DEBUG-ON is set and displays the PA-ACCT-ID if it is. After the delete operation, it checks the DIBSTAT. If DIBSTAT is spaces (successful delete), it increments WS-NO-DTL-DELETED. If the delete fails (DIBSTAT is not spaces), it displays an error message including the DIBSTAT and PA-ACCT-ID and then performs 9999-ABEND to terminate the program.
- Calls: 9999-ABEND
- Lines: 303-323

### 5000-EXIT
**Purpose:** This paragraph serves as the exit point for the 5000-DELETE-AUTH-DTL paragraph. It simply contains the EXIT statement, which returns control to the calling paragraph.
- Lines: 324-325

### 6000-DELETE-AUTH-SUMMARY
**Purpose:** This paragraph deletes an authorization summary segment (PAUTSUM0) from the IMS database. It uses PAUT-PCB-NUM to specify the PCB and PENDING-AUTH-SUMMARY as the source of the segment to be deleted. Before deleting, it checks if DEBUG-ON is set and displays the PA-ACCT-ID if it is. After the delete operation, it checks the DIBSTAT. If DIBSTAT is spaces (successful delete), it increments WS-NO-SUMRY-DELETED. If the delete fails (DIBSTAT is not spaces), it displays an error message including the DIBSTAT and PA-ACCT-ID and then performs 9999-ABEND to terminate the program.
- Calls: 9999-ABEND
- Lines: 328-347

### 6000-EXIT
**Purpose:** This paragraph serves as the exit point for the 6000-DELETE-AUTH-SUMMARY paragraph. It simply contains the EXIT statement, which returns control to the calling paragraph.
- Lines: 348-349

### 9000-TAKE-CHECKPOINT
**Purpose:** This paragraph takes a checkpoint using the IMS CHKP call. It uses WK-CHKPT-ID as the checkpoint ID. After the checkpoint call, it checks the DIBSTAT. If DIBSTAT is spaces (successful checkpoint), it increments WS-NO-CHKP. If WS-NO-CHKP is greater than or equal to P-CHKP-DIS-FREQ, it resets WS-NO-CHKP to 0 and displays a checkpoint success message including WS-NO-SUMRY-READ and WS-CURR-APP-ID. If the checkpoint fails (DIBSTAT is not spaces), it displays an error message including the DIBSTAT, WS-NO-SUMRY-READ, and WS-CURR-APP-ID, and then performs 9999-ABEND to terminate the program.
- Calls: 9999-ABEND
- Lines: 352-372

### 9000-EXIT
**Purpose:** This paragraph serves as the exit point for the 9000-TAKE-CHECKPOINT paragraph. It simply contains the EXIT statement, which returns control to the calling paragraph.
- Lines: 373-374

### 9999-ABEND
**Purpose:** This paragraph handles program termination due to an error. It displays a message 'CBPAUP0C ABENDING ...', moves 16 to RETURN-CODE, and then executes GOBACK to terminate the program.
- Called by: 5000-DELETE-AUTH-DTL, 6000-DELETE-AUTH-SUMMARY, 9000-TAKE-CHECKPOINT
- Lines: 377-383

### 9999-EXIT
**Purpose:** This paragraph serves as an exit point. It simply contains the EXIT statement.
- Lines: 385-386

### CBPAUP0C
**Purpose:** This paragraph, named after the program itself, serves as the entry point. However, based on the provided code snippet, it does not contain any executable statements or logic. It essentially declares the program's existence but performs no actions. There are no inputs consumed, outputs produced, business logic implemented, error handling mechanisms, or calls to other paragraphs or programs within this paragraph. The program terminates immediately after the program ID declaration.
- Lines: 23-23

## Error Handling

- **DIBSTAT not equal to '  ', 'GB', 'GE':** DISPLAY error message and ABEND
  (Lines: 107, 110, 138, 141)

## Open Questions

- **What is the purpose of the 5000-DELETE-AUTH-DTL and 6000-DELETE-AUTH-SUMMARY paragraphs?**
  - Context: The source code does not provide the implementation details for these paragraphs.
  - Suggestion: Examine the source code for the 5000-DELETE-AUTH-DTL and 6000-DELETE-AUTH-SUMMARY paragraphs to understand their functionality.

## Resolved Questions

- **Q:** What is the structure of the PRM-INFO parameter?
  **A:** The structure of the `PRM-INFO` parameter is defined in the COBOL program `CBPAUP0C.cbl` as follows (line 98):

```cobol
01 PRM-INFO.
    05 P-EXPIRY-DAYS          PIC 9(02).
    05 FILLER                 PIC X(01).
    05 P-CHKP-FREQ            PIC X(05).
    05 FILLER                 PIC X(01).
    05 P-CHKP-DIS-FREQ        PIC X(05).
```

This indicates that `PRM-INFO` is a group item containing:
- `P-EXPIRY-DAYS`: A 2-digit numeric field (PIC 9(02)) representing the expiry days.
- `FILLER`: A 1-character alphanumeric field (PIC X(01)).
- `P-CHKP-FREQ`: A 5-character alphanumeric field (PIC X(05)) representing the checkpoint frequency.
- `FILLER`: A 1-character alphanumeric field (PIC X(01)).
- `P-CHKP-DIS-FREQ`: A 5-character alphanumeric field (PIC X(05)) representing the checkpoint display frequency.

The program accepts this parameter from SYSIN (line 189): `ACCEPT PRM-INFO FROM SYSIN`.
- **Q:** What is the purpose of the 9000-TAKE-CHECKPOINT paragraph?
  **A:** The 9000-TAKE-CHECKPOINT paragraph executes a DLI CHKP (checkpoint) call. This is done periodically based on `P-CHKP-FREQ` (likely a checkpoint frequency parameter) and at the end of processing. The checkpoint ID is stored in `WK-CHKPT-ID`.
[skill: cbpaup0c-program-purpose]

---
*Generated by War Rig WAR_RIG*