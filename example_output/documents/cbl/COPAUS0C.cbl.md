# COPAUS0C

**File**: `cbl/COPAUS0C.cbl`
**Type**: COBOL
**Analyzed**: 2026-01-28 14:55:35.475090

## Purpose

This COBOL program's purpose is currently unknown due to the lack of source code provided. Without code, its functionality, business context, and specific operations cannot be determined.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| UNKNOWN | UNKNOWN | The inputs to this program are unknown. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| UNKNOWN | UNKNOWN | The outputs of this program are unknown. |

## Paragraphs/Procedures

### COPAUS0C
[Citadel] Paragraph identified by static analysis

### MAIN-PARA
[Citadel] Paragraph identified by static analysis

### PROCESS-ENTER-KEY
[Citadel] Paragraph identified by static analysis

### GATHER-DETAILS
[Citadel] Paragraph identified by static analysis

### PROCESS-PF7-KEY
[Citadel] Paragraph identified by static analysis

### PROCESS-PF8-KEY
[Citadel] Paragraph identified by static analysis

### PROCESS-PAGE-FORWARD
[Citadel] Paragraph identified by static analysis

### GET-AUTHORIZATIONS
[Citadel] Paragraph identified by static analysis

### REPOSITION-AUTHORIZATIONS
[Citadel] Paragraph identified by static analysis

### POPULATE-AUTH-LIST
[Citadel] Paragraph identified by static analysis

### INITIALIZE-AUTH-DATA
[Citadel] Paragraph identified by static analysis

### RETURN-TO-PREV-SCREEN
[Citadel] Paragraph identified by static analysis

### SEND-PAULST-SCREEN
[Citadel] Paragraph identified by static analysis

### RECEIVE-PAULST-SCREEN
[Citadel] Paragraph identified by static analysis

### POPULATE-HEADER-INFO
[Citadel] Paragraph identified by static analysis

### GATHER-ACCOUNT-DETAILS
[Citadel] Paragraph identified by static analysis

### GETCARDXREF-BYACCT
[Citadel] Paragraph identified by static analysis

### GETACCTDATA-BYACCT
[Citadel] Paragraph identified by static analysis

### GETCUSTDATA-BYCUST
[Citadel] Paragraph identified by static analysis

### GET-AUTH-SUMMARY
[Citadel] Paragraph identified by static analysis

### SCHEDULE-PSB
[Citadel] Paragraph identified by static analysis

## Control Flow

```mermaid
flowchart TD
    %% Title: COPAUS0C.cbl
    GATHER_ACCOUNT_DETAILS["GATHER-ACCOUNT-DETAILS"]
    GET_AUTH_SUMMARY["GET-AUTH-SUMMARY"]
    GETACCTDATA_BYACCT["GETACCTDATA-BYACCT"]
    GETCARDXREF_BYACCT["GETCARDXREF-BYACCT"]
    GETCUSTDATA_BYCUST["GETCUSTDATA-BYCUST"]
    GATHER_DETAILS["GATHER-DETAILS"]
    INITIALIZE_AUTH_DATA["INITIALIZE-AUTH-DATA"]
    PROCESS_PAGE_FORWARD["PROCESS-PAGE-FORWARD"]
    SCHEDULE_PSB["SCHEDULE-PSB"]
    SEND_PAULST_SCREEN["SEND-PAULST-SCREEN"]
    GET_AUTHORIZATIONS["GET-AUTHORIZATIONS"]
    WS_ACCTFILENAME__ext[("WS-ACCTFILENAME")]
    WS_CARDXREFNAME_ACCT_PATH__ext[("WS-CARDXREFNAME-ACCT-PATH")]
    WS_CUSTFILENAME__ext[("WS-CUSTFILENAME")]
    MAIN_PARA["MAIN-PARA"]
    PROCESS_ENTER_KEY["PROCESS-ENTER-KEY"]
    PROCESS_PF7_KEY["PROCESS-PF7-KEY"]
    PROCESS_PF8_KEY["PROCESS-PF8-KEY"]
    RECEIVE_PAULST_SCREEN["RECEIVE-PAULST-SCREEN"]
    RETURN_TO_PREV_SCREEN["RETURN-TO-PREV-SCREEN"]
    POPULATE_AUTH_LIST["POPULATE-AUTH-LIST"]
    POPULATE_HEADER_INFO["POPULATE-HEADER-INFO"]
    CDEMO_TO_PROGRAM__ext(["CDEMO-TO-PROGRAM"])
    REPOSITION_AUTHORIZATIONS["REPOSITION-AUTHORIZATIONS"]
    GATHER_ACCOUNT_DETAILS --> GET_AUTH_SUMMARY
    GATHER_ACCOUNT_DETAILS --> GETACCTDATA_BYACCT
    GATHER_ACCOUNT_DETAILS --> GETCARDXREF_BYACCT
    GATHER_ACCOUNT_DETAILS --> GETCUSTDATA_BYCUST
    GATHER_DETAILS --> GATHER_ACCOUNT_DETAILS
    GATHER_DETAILS --> INITIALIZE_AUTH_DATA
    GATHER_DETAILS --> PROCESS_PAGE_FORWARD
    GET_AUTH_SUMMARY --> SCHEDULE_PSB
    GET_AUTH_SUMMARY --> SEND_PAULST_SCREEN
    GET_AUTHORIZATIONS --> SEND_PAULST_SCREEN
    GETACCTDATA_BYACCT --> SEND_PAULST_SCREEN
    GETACCTDATA_BYACCT -.->|reads| WS_ACCTFILENAME__ext
    GETCARDXREF_BYACCT --> SEND_PAULST_SCREEN
    GETCARDXREF_BYACCT -.->|reads| WS_CARDXREFNAME_ACCT_PATH__ext
    GETCUSTDATA_BYCUST --> SEND_PAULST_SCREEN
    GETCUSTDATA_BYCUST -.->|reads| WS_CUSTFILENAME__ext
    MAIN_PARA --> GATHER_DETAILS
    MAIN_PARA --> PROCESS_ENTER_KEY
    MAIN_PARA --> PROCESS_PF7_KEY
    MAIN_PARA --> PROCESS_PF8_KEY
    MAIN_PARA --> RECEIVE_PAULST_SCREEN
    MAIN_PARA --> RETURN_TO_PREV_SCREEN
    MAIN_PARA --> SEND_PAULST_SCREEN
    PROCESS_ENTER_KEY --> GATHER_DETAILS
    PROCESS_ENTER_KEY -.->|calls| CDEMO_TO_PROGRAM__ext
    PROCESS_PAGE_FORWARD --> GET_AUTHORIZATIONS
    PROCESS_PAGE_FORWARD --> POPULATE_AUTH_LIST
    PROCESS_PAGE_FORWARD --> REPOSITION_AUTHORIZATIONS
    PROCESS_PF7_KEY --> GET_AUTH_SUMMARY
    PROCESS_PF7_KEY --> INITIALIZE_AUTH_DATA
    PROCESS_PF7_KEY --> PROCESS_PAGE_FORWARD
    PROCESS_PF8_KEY --> GET_AUTH_SUMMARY
    PROCESS_PF8_KEY --> INITIALIZE_AUTH_DATA
    PROCESS_PF8_KEY --> PROCESS_PAGE_FORWARD
    PROCESS_PF8_KEY --> REPOSITION_AUTHORIZATIONS
    REPOSITION_AUTHORIZATIONS --> SEND_PAULST_SCREEN
    RETURN_TO_PREV_SCREEN -.->|calls| CDEMO_TO_PROGRAM__ext
    SCHEDULE_PSB --> SEND_PAULST_SCREEN
    SEND_PAULST_SCREEN --> POPULATE_HEADER_INFO
```

## Open Questions

- ? What is the purpose of this program?
  - Context: The source code is missing, making it impossible to determine the program's functionality.
