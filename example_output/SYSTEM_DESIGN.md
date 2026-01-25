# System Design

## 1. Executive Summary

The Card Authorization and Pending Transaction Management System is a mission-critical mainframe application designed to handle the complex lifecycle of credit card authorizations within the CardDemo ecosystem. Its primary purpose is to provide a robust, high-performance bridge between real-time transaction requests and the long-term financial records of the bank. The system solves the business problem of managing "pending" transactions—those that have been authorized but not yet fully cleared or settled—ensuring that customer credit limits are accurately reflected and fraud risks are mitigated in real-time. Stakeholders include bank risk officers, customer service representatives, and external merchants who rely on accurate authorization decisions.

Functionally, the system manages a sophisticated workflow that begins with the ingestion of authorization requests via IBM MQ messaging. These requests are processed by a decision engine that evaluates account status, customer profiles, and cross-reference data to approve or decline transactions. Once an authorization is created, it is stored in a hierarchical IMS database, where it remains until it is either cleared, manually flagged as fraudulent, or automatically purged by batch maintenance routines. The system provides online CICS interfaces for bank staff to search, view, and manage these pending authorizations, including a specialized workflow for marking suspicious transactions for fraud investigation.

Technically, the system is built on a foundation of COBOL and CICS for online processing, utilizing IBM MQ for asynchronous integration and IMS DB for high-availability hierarchical data storage. Batch operations are orchestrated via JCL, employing IMS BMP and DLI modes to perform heavy-duty data loading, unloading, and maintenance tasks. The data architecture is hybrid, leveraging VSAM for cross-reference lookups, IMS for transaction state management, and DB2 for persistent fraud reporting. This multi-tier approach ensures that the system can handle high transaction volumes while maintaining strict data integrity and auditability.

The system's boundaries are well-defined, with inputs arriving as MQ messages from external payment gateways and outputs consisting of MQ responses, updated database records, and flat-file extracts for downstream reporting. It integrates deeply with the broader CardDemo environment, sharing common copybooks and linking to external modules for navigation and specialized fraud logic. If this system were to become unavailable, the bank would lose its ability to process new credit card transactions and manage existing pending authorizations, leading to immediate financial loss, merchant dissatisfaction, and significant reputational damage. It directly supports key business metrics including transaction success rates, fraud detection latency, and system uptime.

## 2. Architecture Overview

The system follows a classic mainframe multi-tier architecture, separating concerns between real-time message processing, online user interaction, and batch data management.

### Architectural Patterns
- **Message-Driven Processing**: [COPAUA0C](cbl/COPAUA0C.md) acts as a listener/processor for MQ queues, implementing an asynchronous request-response pattern.
- **Hierarchical Data Management**: The system uses IMS DB to represent the natural hierarchy of financial data (Account Summary -> Transaction Details).
- **Online/Batch Separation**: CICS programs handle user-facing tasks, while JCL-driven COBOL programs handle bulk data movements and cleanup.
- **Shared Service Layer**: Common logic for screen navigation and fraud processing is encapsulated in external modules like `CDEMO-TO-PROGRAM`.

### Actual System Call Graph

```mermaid
flowchart TD

    subgraph jobs[" "]
        COPAUA0C([COPAUA0C])
        COPAUS0C([COPAUS0C])
        COPAUS1C([COPAUS1C])
        DBPAUTP0([DBPAUTP0])
        DBUNLDGS([DBUNLDGS])
        PAUDBLOD([PAUDBLOD])
        PAUDBUNL([PAUDBUNL])
        UNLDPADB([UNLDPADB])
    end

    subgraph external[" "]
        CBLTDLI>CBLTDLI]
        CDEMO_TO_PROGRAM>CDEMO-TO-PROGRAM]
        DFSRRC00>DFSRRC00]
        MQCLOSE>MQCLOSE]
        MQGET>MQGET]
        MQOPEN>MQOPEN]
        MQPUT1>MQPUT1]
        WS_PGM_AUTH_FRAUD>WS-PGM-AUTH-FRAUD]
    end

    %% Call relationships
    COPAUA0C --> MQOPEN
    COPAUA0C --> MQGET
    COPAUA0C --> MQPUT1
    COPAUA0C --> MQCLOSE
    COPAUS0C --> CDEMO_TO_PROGRAM
    COPAUS0C --> CDEMO_TO_PROGRAM
    COPAUS1C --> WS_PGM_AUTH_FRAUD
    COPAUS1C --> CDEMO_TO_PROGRAM
    DBPAUTP0 --> DFSRRC00
    DBUNLDGS --> CBLTDLI
    DBUNLDGS --> CBLTDLI
    DBUNLDGS --> CBLTDLI
    DBUNLDGS --> CBLTDLI
    PAUDBLOD --> CBLTDLI
    PAUDBLOD --> CBLTDLI
    PAUDBLOD --> CBLTDLI
    PAUDBUNL --> CBLTDLI
    PAUDBUNL --> CBLTDLI
    UNLDPADB --> DFSRRC00
    UNLDPADB --> DFSRRC00
    UNLDPADB --> DFSRRC00
    UNLDPADB --> DFSRRC00

    %% Styling
    classDef entryPoint fill:#90EE90,stroke:#228B22
    class CBPAUP0C,CBPAUP0J,COPAUA0C,COPAUS0C,COPAUS1C,COPAUS2C,DBPAUTP0,DBUNLDGS,LOADPADB,PAUDBLOD,PAUDBUNL,UNLDGSAM,UNLDPADB entryPoint
    classDef missing fill:#1E3A5F,stroke:#2E5A8F,color:#FFFFFF
    class CBLTDLI,CDEMO_TO_PROGRAM,DFSRRC00,MQCLOSE,MQGET,MQOPEN,MQPUT1,WS_PGM_AUTH_FRAUD missing
```

## 3. Component Catalog

| Component | Type | Purpose | Dependencies | Doc Link |
|-----------|------|---------|--------------|----------|
| **CBPAUP0C** | COBOL | Batch IMS cleanup of expired authorizations | [CIPAUDTY](cpy/CIPAUDTY.md), [CIPAUSMY](cpy/CIPAUSMY.md) | [CBPAUP0C](cbl/CBPAUP0C.md) |
| **CBPAUP0J** | JCL | Job to execute CBPAUP0C in BMP mode | [CBPAUP0C](cbl/CBPAUP0C.md), [PSBPAUTB](ims/PSBPAUTB.md) | [CBPAUP0J](jcl/CBPAUP0J.md) |
| **COPAUA0C** | COBOL | MQ-based Authorization Decision Engine | [MQSeries], [CIPAUDTY](cpy/CIPAUDTY.md) | [COPAUA0C](cbl/COPAUA0C.md) |
| **COPAUS0C** | COBOL | Online Summary View of Pending Auths | [COPAU00](bms/COPAU00.md), [CIPAUSMY](cpy/CIPAUSMY.md) | [COPAUS0C](cbl/COPAUS0C.md) |
| **COPAUS1C** | COBOL | Online Detail View and Fraud Flagging | [COPAU01](bms/COPAU01.md), [CIPAUDTY](cpy/CIPAUDTY.md) | [COPAUS1C](cbl/COPAUS1C.md) |
| **COPAUS2C** | COBOL | DB2 Fraud Record Insertion/Update | [AUTHFRDS](ddl/AUTHFRDS.md), [CIPAUDTY](cpy/CIPAUDTY.md) | [COPAUS2C](cbl/COPAUS2C.md) |
| **DBPAUTP0** | JCL | Unload IMS DB to sequential file | [DFSRRC00], [DBPAUTP0](ims/DBPAUTP0.md) | [DBPAUTP0](jcl/DBPAUTP0.md) |
| **DBUNLDGS** | COBOL | IMS to GSAM Database Migration Utility | [DLIGSAMP](ims/DLIGSAMP.md), [PAUTBPCB](cpy/PAUTBPCB.md) | [DBUNLDGS](cbl/DBUNLDGS.md) |
| **LOADPADB** | JCL | Load PAUDB IMS Database | [PAUDBLOD](cbl/PAUDBLOD.md), [PSBPAUTB](ims/PSBPAUTB.md) | [LOADPADB](jcl/LOADPADB.md) |
| **PAUDBLOD** | COBOL | Batch IMS Database Loader | [CBLTDLI], [CIPAUDTY](cpy/CIPAUDTY.md) | [PAUDBLOD](cbl/PAUDBLOD.md) |
| **PAUDBUNL** | COBOL | Batch IMS Database Unloader | [CBLTDLI], [CIPAUSMY](cpy/CIPAUSMY.md) | [PAUDBUNL](cbl/PAUDBUNL.md) |
| **UNLDGSAM** | JCL | Process GSAM datasets for PAUTDB | [DFSRRC00], [DLIGSAMP](ims/DLIGSAMP.md) | [UNLDGSAM](jcl/UNLDGSAM.md) |
| **UNLDPADB** | JCL | Unload root/child segments to flat files | [PAUDBUNL](cbl/PAUDBUNL.md), [PAUTBUNL](ims/PAUTBUNL.md) | [UNLDPADB](jcl/UNLDPADB.md) |
| **COPAU00** | BMS | Map for Authorization Summary Screen | N/A | [COPAU00](bms/COPAU00.md) |
| **COPAU01** | BMS | Map for Authorization Detail Screen | N/A | [COPAU01](bms/COPAU01.md) |
| **PADFLDBD** | DBD | IMS Database Definition (Detail) | N/A | [PADFLDBD](ims/PADFLDBD.md) |
| **PASFLDBD** | DBD | IMS Database Definition (Summary) | N/A | [PASFLDBD](ims/PASFLDBD.md) |
| **XAUTHFRD** | DDL | Index for Fraud Table | [AUTHFRDS](ddl/AUTHFRDS.md) | [XAUTHFRD](ddl/XAUTHFRD.md) |

## 4. Subsystem Breakdown

### 4.1 Authorization Decision Subsystem (Online/MQ)
This subsystem handles the real-time processing of incoming transaction requests.
- **Primary Program**: [COPAUA0C](cbl/COPAUA0C.md)
- **Workflow**:
    1. Read request from MQ.
    2. Validate card via VSAM XREF.
    3. Check account/customer status via VSAM.
    4. Query IMS for existing pending totals.
    5. Apply business rules to Approve/Decline.
    6. Update IMS with new authorization.
    7. Reply via MQ.

### 4.2 Online Management Subsystem (CICS)
Provides the user interface for bank staff to monitor and manage pending transactions.
- **Summary View**: [COPAUS0C](cbl/COPAUS0C.md) displays a list of authorizations for an account using [COPAU00](bms/COPAU00.md).
- **Detail View**: [COPAUS1C](cbl/COPAUS1C.md) provides deep-dive into a single transaction using [COPAU01](bms/COPAU01.md).
- **Fraud Action**: [COPAUS2C](cbl/COPAUS2C.md) records fraud flags into DB2 via [AUTHFRDS](ddl/AUTHFRDS.md).

### 4.3 Batch Maintenance & ETL Subsystem
Handles the lifecycle of data within the IMS databases.
- **Cleanup**: [CBPAUP0C](cbl/CBPAUP0C.md) purges expired records based on business parameters.
- **Loading**: [PAUDBLOD](cbl/PAUDBLOD.md) populates the database from flat files.
- **Unloading/Backup**: [PAUDBUNL](cbl/PAUDBUNL.md) and [DBPAUTP0](jcl/DBPAUTP0.md) extract data for archival or reporting.
- **Migration**: [DBUNLDGS](cbl/DBUNLDGS.md) moves data between standard IMS and GSAM structures.

## 5. Data Architecture

### 5.1 Data Stores
- **IMS DB (PAUTDB)**: Hierarchical store for pending authorizations.
    - **Root Segment (PAUTSUM0)**: Summary of authorizations per account.
    - **Child Segment (PAUTDTL1)**: Individual transaction details.
- **DB2 (CARDDEMO.AUTHFRDS)**: Relational table for persistent fraud reporting and audit trails.
- **VSAM KSDS**: Used for Account, Customer, and Card Cross-Reference lookups.
- **GSAM**: Sequential access to IMS data for batch processing.

### 5.2 Key Data Structures
- [CIPAUDTY](cpy/CIPAUDTY.md): Defines the layout for Authorization Detail records.
- [CIPAUSMY](cpy/CIPAUSMY.md): Defines the layout for Authorization Summary records.
- [PAUTBPCB](cpy/PAUTBPCB.md): IMS Program Communication Block for database access.

## 6. Integration Points

- **MQSeries**: Primary interface for external transaction requests. Uses `MQGET` for requests and `MQPUT1` for responses.
- **CICS Link**: Online programs use `EXEC CICS LINK` to communicate with shared modules like `WS-PGM-AUTH-FRAUD`.
- **IMS DLI/BMP**: Batch jobs interface with IMS via `CBLTDLI` calls or the `DFSRRC00` utility.
- **DB2 SQL**: [COPAUS2C](cbl/COPAUS2C.md) uses standard SQL for interacting with the fraud table.

## 7. Business Rules Summary

- **Expiration Logic**: [CBPAUP0C](cbl/CBPAUP0C.md) identifies transactions older than a configurable threshold and removes them to free up credit limits.
- **Authorization Decision**: [COPAUA0C](cbl/COPAUA0C.md) evaluates multiple factors (account status, credit limit, card validity) before issuing an approval code.
- **Fraud Handling**: [COPAUS2C](cbl/COPAUS2C.md) implements an "Upsert" logic—if a fraud record exists, update it; otherwise, insert a new one.
- **Data Integrity**: Batch loaders ([PAUDBLOD](cbl/PAUDBLOD.md)) must ignore duplicate segments to prevent data corruption during re-runs.

## 8. Error Handling Patterns

- **IMS Status Codes**: Programs check for 'GB' (End of Database), 'GE' (Not Found), and 'II' (Duplicate) to manage flow.
- **SQLCODE Validation**: [COPAUS2C](cbl/COPAUS2C.md) specifically handles `-803` (Duplicate Key) to switch from Insert to Update mode.
- **MQ Completion Codes**: [COPAUA0C](cbl/COPAUA0C.md) monitors MQCC and MQRC to handle messaging failures.
- **Abend Strategy**: Batch programs like [PAUDBUNL](cbl/PAUDBUNL.md) use a `9999-ABEND` routine to halt processing on critical database errors.

## 9. Open Questions and Uncertainties

- ❓ QUESTION: What is the specific retention period (in days) used by [CBPAUP0C](cbl/CBPAUP0C.md) for expiration?
- ❓ QUESTION: Are the MQ queues used by [COPAUA0C](cbl/COPAUA0C.md) persistent or non-persistent?
- ❓ QUESTION: The program `CDEMO-TO-PROGRAM` is called frequently for navigation; is this a standard CardDemo router or a custom implementation?
- ❓ QUESTION: Why are the sequential file outputs in [DBUNLDGS](cbl/DBUNLDGS.md) commented out? Is this program still in active use or replaced by GSAM?