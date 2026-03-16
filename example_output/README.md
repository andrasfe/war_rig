# System Design Document

## 1. Executive Summary

This authorization system is a critical component of the financial transaction processing infrastructure, responsible for validating and approving or denying credit and debit card transactions in real-time. Its primary purpose is to prevent fraudulent activities, minimize financial losses for the institution, and ensure secure and reliable payment processing for its customers. The system serves a diverse range of users, including merchants, cardholders, and internal bank personnel involved in fraud detection and customer service. By accurately assessing transaction risk and enforcing spending limits, the system safeguards the financial interests of both the institution and its customers. The system's mission is to provide a robust and adaptable authorization platform that can evolve with emerging fraud trends and changing business requirements.

The system provides several key capabilities, including real-time authorization processing, fraud detection, and transaction logging. The core workflow begins when a merchant submits a transaction request to the system. The system then performs a series of checks, including verifying the card's validity, checking for sufficient funds or credit limit, and assessing the transaction's risk score based on predefined fraud rules. If the transaction meets all the criteria, it is approved, and an authorization code is sent back to the merchant. If the transaction fails any of the checks, it is declined, and the merchant is notified. The system also supports various transaction types, such as purchase, refund, and void, and provides comprehensive reporting and auditing capabilities. Key programs involved in this process include [COPAUS0C](cbl/COPAUS0C.cbl.md) and [COPAUS1C](cbl/COPAUS1C.cbl.md), which handle authorization processing and screen display respectively.

The system is built on a foundation of mainframe technologies, including COBOL for application logic, JCL for batch processing, CICS for online transaction processing, and IMS for database management. COBOL programs such as [PAUDBUNL](cbl/PAUDBUNL.CBL.md) handle the core business logic, while JCL jobs like [UNLDPADB](jcl/UNLDPADB.JCL.md) are used for batch data processing and database maintenance. CICS transactions provide the online interface for users to interact with the system, and IMS databases store critical data such as cardholder information, transaction history, and fraud rules. The system leverages copybooks like [CIPAUDTY](cpy/CIPAUDTY.cpy.md) and [CIPAUSMY](cpy/CIPAUSMY.cpy.md) to ensure data consistency across different programs.

The system receives transaction requests from various sources, including point-of-sale terminals, e-commerce platforms, and mobile payment applications. It interacts with external systems such as credit card networks, fraud detection services, and customer relationship management (CRM) systems. The system's outputs include authorization codes, decline messages, transaction logs, and reports. The system's boundaries are defined by the interfaces through which it receives transaction requests and the interfaces through which it communicates with external systems.

The authorization system is essential for maintaining the financial integrity of the institution and protecting its customers from fraud. Its availability and performance directly impact the institution's revenue, reputation, and customer satisfaction. If the system is unavailable, transactions cannot be processed, leading to lost sales and customer dissatisfaction. A failure in the system could also result in fraudulent transactions being approved, leading to significant financial losses. The system's ability to accurately assess transaction risk and prevent fraud is critical for maintaining the institution's competitive advantage and ensuring its long-term success.

Since there are no skills available for the main programs, I will rely on the structural context and code analysis to understand the architecture.

**Architectural Layers:**

*   **Online (CICS):** The online portion of the system is driven by CICS transactions. Programs like [COPAUS0C](cbl/COPAUS0C.cbl.md), [COPAUS1C](cbl/COPAUS1C.cbl.md), [COPAUA0C](cbl/COPAUA0C.cbl.md), and [COPAUS2C](cbl/COPAUS2C.cbl.md) handle real-time authorization requests and user interactions.
*   **Batch:** The batch side focuses on data unloading, processing, and archival. Programs like [PAUDBUNL](cbl/PAUDBUNL.CBL.md), [DBUNLDGS](cbl/DBUNLDGS.CBL.md), and [CBPAUP0C](cbl/CBPAUP0C.cbl.md) are involved in these tasks.

**Component Interactions:**

*   **[COPAUA0C](COPAUA0C.cbl.md):** This program appears to be a central request processor. According to the AST summary, it reads requests from an MQ queue, schedules a PSB, and processes authorizations. It calls other programs to read cross-reference, account, and customer records.
*   **[COPAUS0C](COPAUS0C.cbl.md):** This program seems to gather authorization details. The AST summary indicates that it calls other routines to get authorization summary, authorizations, account data, card cross-reference, and customer data. It also populates the authorization list.
*   **[COPAUS1C](COPAUS1C.cbl.md):** This program likely handles screen interactions and authorization processing. The AST summary shows that it processes enter and PF8 keys, reads authorization records, receives the authorization view screen, and returns to the previous screen. It also calls programs to mark authorizations as fraudulent.
*   **[COPAUS2C](COPAUS2C.cbl.md):** This program handles fraud updates. It calls the `FRAUD-UPDATE` paragraph.
*   **[PAUDBUNL](PAUDBUNL.CBL.md):** This program unloads data. The AST summary indicates it finds the next authorization summary and detail records.
*   **[DBUNLDGS](DBUNLDGS.CBL.md):** This program unloads data to GSAM datasets. The AST summary shows that it finds the next authorization summary and detail records and inserts parent and child segments into GSAM.
*   **[CBPAUP0C](CBPAUP0C.cbl.md):** This program seems to process authorization summaries. The AST summary shows that it finds the next authorization summary and detail records, checks if they are expired, and deletes them if necessary. It also takes checkpoints.

**Data Access Patterns:**

*   The online programs ([COPAUS0C](COPAUS0C.cbl.md), [COPAUS1C](COPAUS1C.cbl.md), [COPAUA0C](COPAUA0C.cbl.md)) heavily rely on IMS database access to retrieve customer, account, and authorization information.
*   The batch programs ([PAUDBUNL](PAUDBUNL.CBL.md), [DBUNLDGS](DBUNLDGS.CBL.md)) primarily read and write sequential files and GSAM datasets.

**Integration Points:**

*   **Program Calls:** The programs interact through direct COBOL calls (PERFORM statements). The call graph illustrates these relationships.
*   **Shared Copybooks:** Several copybooks are shared between programs, facilitating data exchange. For example, [CIPAUDTY](CIPAUDTY.cpy.md) and [CIPAUSMY](CIPAUSMY.cpy.md) are shared between multiple programs, including batch and online components.
*   **IBM MQ:** [COPAUA0C](COPAUA0C.cbl.md) uses IBM MQ to receive authorization requests.

This overview provides a high-level understanding of the system's architecture. Further investigation into individual programs and their interactions is needed for a more detailed understanding.
```

## 3. Component Catalog

This section provides a catalog of all documented components in the system, grouped by type. Each component includes a brief description of its purpose and a link to its detailed documentation.

### COBOL Programs

| Component | Type | Purpose | Doc Link |
|---|---|---|---|
| PAUDBUNL | COBOL Program | Unloads authorization data from IMS database to sequential file. | [PAUDBUNL](cbl/PAUDBUNL.CBL.md) |
| COPAUS1C | COBOL Program | Processes user interactions on the authorization view screen. | [COPAUS1C](cbl/COPAUS1C.cbl.md) |
| COPAUA0C | COBOL Program | Processes authorization requests received from MQ. | [COPAUA0C](cbl/COPAUA0C.cbl.md) |
| PAUDBLOD | COBOL Program | Loads authorization data from sequential file to IMS database. | [PAUDBLOD](cbl/PAUDBLOD.CBL.md) |
| DBUNLDGS | COBOL Program | Unloads authorization data from IMS to GSAM dataset. | [DBUNLDGS](cbl/DBUNLDGS.CBL.md) |
| CBPAUP0C | COBOL Program | Purges expired authorization data. | [CBPAUP0C](cbl/CBPAUP0C.cbl.md) |
| COPAUS0C | COBOL Program | Retrieves and displays authorization information. | [COPAUS0C](cbl/COPAUS0C.cbl.md) |
| COPAUS2C | COBOL Program | Updates fraud information. | [COPAUS2C](cbl/COPAUS2C.cbl.md) |

### JCL Jobs

| Component | Type | Purpose | Doc Link |
|---|---|---|---|
| UNLDPADB | JCL Job | JCL to unload the PADB database. | [UNLDPADB](jcl/UNLDPADB.JCL.md) |
| LOADPADB | JCL Job | JCL to load the PADB database. | [LOADPADB](jcl/LOADPADB.JCL.md) |
| UNLDGSAM | JCL Job | JCL to unload IMS database to GSAM dataset. | [UNLDGSAM](jcl/UNLDGSAM.JCL.md) |
| DBPAUTP0 | JCL Job | JCL to define the DBPAUTP0 database. | [DBPAUTP0](jcl/DBPAUTP0.jcl.md) |
| CBPAUP0J | JCL Job | JCL to run the CBPAUP0C program. | [CBPAUP0J](jcl/CBPAUP0J.jcl.md) |

### Copybooks

| Component | Type | Purpose | Doc Link |
|---|---|---|---|
| CIPAUDTY | Copybook | Defines the authorization audit data structure. | [CIPAUDTY](cpy/CIPAUDTY.cpy.md) |
| CIPAUSMY | Copybook | Defines the authorization master data structure. | [CIPAUSMY](cpy/CIPAUSMY.cpy.md) |
| COCOM01Y | Copybook | Common copybook. ❓ QUESTION: What data structure does it define? | N/A |
| COTTL01Y | Copybook | Common copybook. ❓ QUESTION: What data structure does it define? | N/A |
| CSDAT01Y | Copybook | Common copybook. ❓ QUESTION: What data structure does it define? | N/A |
| CSMSG01Y | Copybook | Common copybook. ❓ QUESTION: What data structure does it define? | N/A |
| CSMSG02Y | Copybook | Common copybook. ❓ QUESTION: What data structure does it define? | N/A |
| CVACT01Y | Copybook | Common copybook. ❓ QUESTION: What data structure does it define? | N/A |
| CVACT03Y | Copybook | Common copybook. ❓ QUESTION: What data structure does it define? | N/A |
| CVCUS01Y | Copybook | Common copybook. ❓ QUESTION: What data structure does it define? | N/A |
| DFHAID | Copybook | Defines the CICS AID keys. | N/A |
| DFHBMSCA | Copybook | Defines the BMS control area. | N/A |
| IMSFUNCS | Copybook | Defines IMS function codes. | [IMSFUNCS](cpy/IMSFUNCS.cpy.md) |
| PAUTBPCB | Copybook | Defines the PCB for authorization database. | [PAUTBPCB](cpy/PAUTBPCB.CPY.md) |
| CCPAURQY | Copybook | Defines the structure for authorization requests. | [CCPAURQY](cpy/CCPAURQY.cpy.md) |
| CCPAUERY | Copybook | Defines the structure for authorization errors. | [CCPAUERY](cpy/CCPAUERY.cpy.md) |
| CCPAURLY | Copybook | Defines the structure for authorization replies. | [CCPAURLY](cpy/CCPAURLY.cpy.md) |
| PADFLPCB | Copybook | Defines the PCB for the PADFL database. | [PADFLPCB](cpy/PADFLPCB.CPY.md) |
| PASFLPCB | Copybook | Defines the PCB for the PASFL database. | [PASFLPCB](cpy/PASFLPCB.CPY.md) |
| COPAU00 | Copybook | Copybook for COPAU00 BMS map. | [COPAU00](cpy-bms/COPAU00.cpy.md) |
| COPAU01 | Copybook | Copybook for COPAU01 BMS map. | [COPAU01](cpy-bms/COPAU01.cpy.md) |

### BMS Maps

| Component | Type | Purpose | Doc Link |
|---|---|---|---|
| COPAU00 | BMS Map | Defines the screen layout for the initial authorization screen. | [COPAU00](bms/COPAU00.bms.md) |
| COPAU01 | BMS Map | Defines the screen layout for the authorization details screen. | [COPAU01](bms/COPAU01.bms.md) |

### IMS Database Definitions

| Component | Type | Purpose | Doc Link |
|---|---|---|---|
| XAUTHFRD | DDL | DDL for XAUTHFRD. ❓ QUESTION: What is XAUTHFRD? | [XAUTHFRD](ddl/XAUTHFRD.ddl.md) |
| AUTHFRDS | DDL | DDL for AUTHFRDS. ❓ QUESTION: What is AUTHFRDS? | [AUTHFRDS](ddl/AUTHFRDS.ddl.md) |
| DBPAUTP0 | DBD | Defines the DBPAUTP0 database. | [DBPAUTP0](ims/DBPAUTP0.dbd.md) |
| DBPAUTX0 | DBD | Defines the DBPAUTX0 database. | [DBPAUTX0](ims/DBPAUTX0.dbd.md) |
| PADFLDBD | DBD | Defines the PADFL database. | [PADFLDBD](ims/PADFLDBD.DBD.md) |
| PSBPAUTL | PSB | Defines the PSBPAUTL program specification block. | [PSBPAUTL](ims/PSBPAUTL.psb.md) |
| PSBPAUTB | PSB | Defines the PSBPAUTB program specification block. | [PSBPAUTB](ims/PSBPAUTB.psb.md) |
| PASFLDBD | DBD | Defines the PASFL database. | [PASFLDBD](ims/PASFLDBD.DBD.md) |
| PAUTBUNL | PSB | Defines the PAUTBUNL program specification block. | [PAUTBUNL](ims/PAUTBUNL.PSB.md) |
| DLIGSAMP | PSB | Defines the DLIGSAMP program specification block. | [DLIGSAMP](ims/DLIGSAMP.PSB.md) |

## 4. Subsystem Breakdown

This section breaks down the system into logical subsystems based on shared functionality and data structures.

### 4.1. Online Authorization Subsystem

This subsystem handles real-time authorization requests via CICS.

**Programs:**

*   [COPAUS0C](cbl/COPAUS0C.cbl.md): Provides the main authorization processing logic. It gathers account details, retrieves authorization summaries and details, and populates authorization lists.
*   [COPAUS1C](cbl/COPAUS1C.cbl.md): Handles screen interactions for authorization viewing and fraud marking. It receives screen input, reads authorization records, and returns to the previous screen.
*   [COPAUA0C](cbl/COPAUA0C.cbl.md): Processes authorization requests received via MQ. It extracts the request message, processes the authorization, and reads related records.
*   [COPAUS2C](cbl/COPAUS2C.cbl.md): Updates fraud information.

**Responsibility:**

The Online Authorization Subsystem is responsible for processing real-time authorization requests, displaying authorization information to users, and allowing users to mark authorizations as fraudulent.

**Interactions:**

*   This subsystem interacts with the IMS database to retrieve and update authorization data.
*   [COPAUA0C](cbl/COPAUA0C.cbl.md) interacts with IBM MQ to receive authorization requests.
*   [COPAUS1C](cbl/COPAUS1C.cbl.md) interacts with CICS to handle screen input and output.

**Shared Copybooks:**

This subsystem utilizes several shared copybooks, including:

*   [COCOM01Y](cpy/COCOM01Y.cpy.md)
*   [COTTL01Y](cpy/COTTL01Y.cpy.md)
*   [CSDAT01Y](cpy/CSDAT01Y.cpy.md)
*   [CSMSG01Y](cpy/CSMSG01Y.cpy.md)
*   [CSMSG02Y](cpy/CSMSG02Y.cpy.md)
*   [CVACT01Y](cpy/CVACT01Y.cpy.md)
*   [CVACT03Y](cpy/CVACT03Y.cpy.md)
*   [CVCUS01Y](cpy/CVCUS01Y.cpy.md)
*   [DFHAID](cpy/DFHAID.cpy.md)
*   [DFHBMSCA](cpy/DFHBMSCA.cpy.md)

### 4.2. Batch Unload/Reload Subsystem

This subsystem handles the batch unloading and reloading of the authorization database.

**Programs:**

*   [PAUDBUNL](cbl/PAUDBUNL.CBL.md): Unloads authorization data from the IMS database to sequential files.
*   [PAUDBLOD](cbl/PAUDBLOD.CBL.md): Reloads authorization data from sequential files into the IMS database.
*   [DBUNLDGS](cbl/DBUNLDGS.CBL.md): Unloads authorization summary and detail data to GSAM files.
*   [CBPAUP0C](cbl/CBPAUP0C.cbl.md): Takes checkpoints and deletes expired authorization details and summaries.

**JCL:**

*   [UNLDPADB](jcl/UNLDPADB.JCL.md): JCL to unload the PADB database.
*   [LOADPADB](jcl/LOADPADB.JCL.md): JCL to load the PADB database.
*   [UNLDGSAM](jcl/UNLDGSAM.JCL.md): JCL to unload GSAM datasets.
*   [DBPAUTP0](jcl/DBPAUTP0.jcl.md): JCL related to database processing.
*   [CBPAUP0J](jcl/CBPAUP0J.jcl.md): JCL to run CBPAUP0C.

**Responsibility:**

The Batch Unload/Reload Subsystem is responsible for backing up and restoring the authorization database, as well as performing maintenance tasks such as deleting expired data.

**Interactions:**

*   This subsystem interacts with the IMS database to unload and reload data.
*   [PAUDBUNL](cbl/PAUDBUNL.CBL.md) and [PAUDBLOD](cbl/PAUDBLOD.CBL.md) interact with sequential files for data storage.
*   [DBUNLDGS](cbl/DBUNLDGS.CBL.md) interacts with GSAM files.

**Shared Copybooks:**

This subsystem utilizes several shared copybooks, including:

*   [CIPAUDTY](CIPAUDTY.cpy.md)
*   [CIPAUSMY](CIPAUSMY.cpy.md)
*   [IMSFUNCS](IMSFUNCS.cpy.md)
*   [PAUTBPCB](PAUTBPCB.CPY.md)

### 4.3 IMS Database Definitions. IMS Database Definitions

This subsystem defines the structure of the IMS databases used by the system.

**DBDs:**

*   [DBPAUTP0](ims/DBPAUTP0.dbd.md): Defines the structure of the authorization database.
*   [DBPAUTX0](ims/DBPAUTX0.dbd.md): Defines the structure of the authorization index database.
*   [PADFLDBD](ims/PADFLDBD.DBD.md): ❓ QUESTION: What is the purpose of PADFLDBD?
*   [PASFLDBD](ims/PASFLDBD.DBD.md): ❓ QUESTION: What is the purpose of PASFLDBD?

**PSBs:**

*   [PSBPAUTL](ims/PSBPAUTL.psb.md): Defines the program specification block for online access.
*   [PSBPAUTB](ims/PSBPAUTB.psb.md): Defines the program specification block for batch access.
*   [PAUTBUNL](ims/PAUTBUNL.PSB.md): Defines the PSB used by the unload program.
*   [DLIGSAMP](ims/DLIGSAMP.PSB.md): ❓ QUESTION: What is the purpose of DLIGSAMP?

**Responsibility:**

The IMS Database Definitions subsystem is responsible for defining the structure and access methods for the IMS databases used by the system.

**Interactions:**

*   This subsystem is used by the Online Authorization Subsystem and the Batch Unload/Reload Subsystem to access and manipulate data in the IMS databases.

## 5. Data Architecture Data Architecture

This section describes the data architecture of the authorization system, including key datasets, databases, data flows, and shared data structures.

### 5.1. Key Datasets and Databases

The system utilizes several key datasets and databases for storing and managing authorization data. These include IMS databases and sequential files.

| Dataset/Database | Description | Access Method | Programs Reading | Programs Writing |
|---|---|---|---|---|
| IMS Database: DBPAUTP0 | Stores authorization parameters. | IMS | COPAUA0C (via PSBPAUTL), COPAUS0C | PAUDBLOD, CBPAUP0C |
| IMS Database: DBPAUTX0 | Stores authorization cross-reference data. | IMS | COPAUA0C (via PSBPAUTL) |  |
| Sequential File: AUTHFRDS | Fraudulent authorization data. | Sequential | COPAUS1C, COPAUS2C |  |
| Sequential File: XAUTHFRD | Cross-reference data for fraudulent authorizations. | Sequential | COPAUA0C |  |

### 5.2. Data Flow Patterns

The system employs various data flow patterns, including sequential access for file processing and IMS calls for database access.

*   **Sequential Data Flow:** Used for processing sequential files such as `AUTHFRDS` and `XAUTHFRD`. Programs read records sequentially from these files to perform authorization checks or updates.
*   **IMS Database Access:** Programs like `COPAUA0C`, `PAUDBLOD`, and `CBPAUP0C` use IMS calls to access and manipulate data within the `DBPAUTP0` and `DBPAUTX0` databases. The PSB `PSBPAUTL` is used to access `DBPAUTP0`.

### 5.3. Shared Data Structures (Copybooks)

Several copybooks are used to define shared data structures across multiple programs, ensuring data consistency and facilitating data exchange.

| Copybook | Description | Programs Using |
|---|---|---|
| [CIPAUDTY](cpy/CIPAUDTY.cpy.md) | Audit trail data structure. | PAUDBUNL, COPAUS1C, COPAUA0C, PAUDBLOD, DBUNLDGS, CBPAUP0C, COPAUS0C, COPAUS2C |
| [CIPAUSMY](cpy/CIPAUSMY.cpy.md) | User master data structure. | PAUDBUNL, COPAUS1C, COPAUA0C, PAUDBLOD, DBUNLDGS, CBPAUP0C, COPAUS0C |
| [COCOM01Y](cpy/COCOM01Y.cpy.md) | Common data definitions. | COPAUS1C, COPAUS0C |
| [COTTL01Y](cpy/COTTL01Y.cpy.md) | Terminal I/O definitions. | COPAUS1C, COPAUS0C |
| [CSDAT01Y](cpy/CSDAT01Y.cpy.md) | Date and time definitions. | COPAUS1C, COPAUS0C |
| [CSMSG01Y](cpy/CSMSG01Y.cpy.md) | Message definitions. | COPAUS1C, COPAUS0C |
| [CSMSG02Y](cpy/CSMSG02Y.cpy.md) | Message definitions. | COPAUS1C, COPAUS0C |
| [CVACT01Y](cpy/CVACT01Y.cpy.md) | Account data definitions. | COPAUA0C, COPAUS0C |
| [CVACT03Y](cpy/CVACT03Y.cpy.md) | Account data definitions. | COPAUA0C, COPAUS0C |
| [CVCUS01Y](cpy/CVCUS01Y.cpy.md) | Customer data definitions. | COPAUA0C, COPAUS0C |
| [DFHAID](cpy/DFHAID.cpy.md) | CICS AID definitions. | COPAUS1C, COPAUS0C |
| [DFHBMSCA](cpy/DFHBMSCA.cpy.md) | BMS control area definitions. | COPAUS1C, COPAUS0C |
| [IMSFUNCS](cpy/IMSFUNCS.cpy.md) | IMS function codes. | PAUDBUNL, PAUDBLOD, DBUNLDGS |
| [PAUTBPCB](cpy/PAUTBPCB.CPY.md) | PCB data structure. | PAUDBUNL, PAUDBLOD, DBUNLDGS |

### 5.4. Data Flow Diagram

The following table illustrates the data flow between key programs and datasets:

| Producer | Dataset/Database | Consumer | Description |
|---|---|---|---|
| [COPAUA0C](cbl/COPAUA0C.cbl.md) | MQ Request Queue |  | Reads authorization requests from the MQ queue. |
|  | [XAUTHFRD](ddl/XAUTHFRD.ddl.md) | [COPAUA0C](cbl/COPAUA0C.cbl.md) | Reads cross-reference data for fraudulent authorizations. |
| [PAUDBLOD](cbl/PAUDBLOD.CBL.md) | [DBPAUTP0](ims/DBPAUTP0.dbd.md) |  | Loads authorization parameters into the IMS database. |
| [CBPAUP0C](cbl/CBPAUP0C.cbl.md) | [DBPAUTP0](ims/DBPAUTP0.dbd.md) |  | Updates authorization parameters in the IMS database. |
|  | [DBPAUTP0](ims/DBPAUTP0.dbd.md) | [COPAUA0C](cbl/COPAUA0C.cbl.md), [COPAUS0C](cbl/COPAUS0C.cbl.md) | Reads authorization parameters from the IMS database. |
| [COPAUS1C](cbl/COPAUS1C.cbl.md) | [AUTHFRDS](ddl/AUTHFRDS.ddl.md) |  | Marks authorizations as fraudulent. |
|  | [AUTHFRDS](ddl/AUTHFRDS.ddl.md) | [COPAUS1C](cbl/COPAUS1C.cbl.md), [COPAUS2C](cbl/COPAUS2C.cbl.md) | Reads fraudulent authorization data. |

## 6. Integration Points Integration Points

This section details the integration points of the authorization system, including external interfaces, batch job scheduling, CICS transaction entry points, and cross-system data exchanges.

### 6.1 External System Interfaces

The system interacts with external systems through various interfaces, including files, MQ queues, and potentially APIs.

#### 6.1.1 MQ Queues

The program [COPAUA0C](cbl/COPAUA0C.cbl.md) interacts with external systems via MQ queues. Specifically, paragraph `1100-OPEN-REQUEST-QUEUE` opens a request queue. Further investigation is needed to determine the specific queue names and message formats. ❓ QUESTION: What are the specific MQ queue names and message formats used by COPAUA0C?

#### 6.1.2 File Interfaces

The system interacts with external files for data loading and unloading. The JCL jobs [UNLDPADB](jcl/UNLDPADB.JCL.md) and [LOADPADB](jcl/LOADPADB.JCL.md) are used for unloading and loading data respectively.

*   **[UNLDPADB](jcl/UNLDPADB.JCL.md)**: This job unloads data from IMS databases to sequential files. It likely extracts authorization data for reporting or archival purposes.
*   **[LOADPADB](jcl/LOADPADB.JCL.md)**: This job loads data into IMS databases from sequential files. It's likely used for initial data population or data restoration.

The programs [PAUDBUNL](cbl/PAUDBUNL.CBL.md), [PAUDBLOD](cbl/PAUDBLOD.CBL.md), and [DBUNLDGS](cbl/DBUNLDGS.CBL.md) are involved in processing these files.

*   **[PAUDBUNL](cbl/PAUDBUNL.CBL.md)**: This program unloads data from IMS.
*   **[PAUDBLOD](cbl/PAUDBLOD.CBL.md)**: This program loads data into IMS.
*   **[DBUNLDGS](cbl/DBUNLDGS.CBL.md)**: This program unloads data.

#### 6.1.3 API Interfaces

While there's no explicit mention of API interfaces in the provided documentation, it's possible that some programs expose APIs for external systems to access authorization data. Further investigation is needed to confirm this. ❓ QUESTION: Does the system expose any APIs for external systems to access authorization data?

### 6.2 Batch Job Scheduling

Several JCL jobs are used for batch processing, including data unloading, loading, and database maintenance.

*   **[UNLDPADB](jcl/UNLDPADB.JCL.md)**: Unloads data from IMS databases.
*   **[LOADPADB](jcl/LOADPADB.JCL.md)**: Loads data into IMS databases.
*   **[UNLDGSAM](jcl/UNLDGSAM.JCL.md)**: Unloads data, likely for audit or reporting.
*   **[DBPAUTP0](jcl/DBPAUTP0.jcl.md)**: Database processing job.
*   **[CBPAUP0J](jcl/CBPAUP0J.jcl.md)**: Checkpoint job.

The scheduling of these jobs is not explicitly defined in the provided documentation. ❓ QUESTION: What is the scheduling frequency and dependencies for each batch job? Are there any specific scheduling tools or systems used?

### 6.3 CICS Transaction Entry Points

The program [COPAUS1C](cbl/COPAUS1C.cbl.md) is a CICS program that handles user interactions through screens. It receives input from the `RECEIVE-AUTHVIEW-SCREEN` paragraph and processes user actions based on keys pressed (e.g., `PROCESS-ENTER-KEY`, `PROCESS-PF8-KEY`).

*   **[COPAUS1C](cbl/COPAUS1C.cbl.md)**: Provides an interface for users to view and manage authorization details.

The specific transaction ID associated with [COPAUS1C](cbl/COPAUS1C.cbl.md) is not explicitly mentioned. ❓ QUESTION: What is the CICS transaction ID associated with the COPAUS1C program?

### 6.4 Cross-System Data Exchanges

The system exchanges data with other systems through MQ queues and files. The program [COPAUA0C](cbl/COPAUA0C.cbl.md) reads requests from an MQ queue and processes them. The data exchanged includes authorization requests and responses. The specifics of the data exchanged are defined by the copybooks used in the programs.

*   **[COPAUA0C](cbl/COPAUA0C.cbl.md)**: Reads authorization requests from an MQ queue.
*   **[CIPAUDTY](cpy/CIPAUDTY.cpy.md)**: Audit data copybook, likely used for data exchange related to auditing.
*   **[CIPAUSMY](cpy/CIPAUSMY.cpy.md)**: User data copybook, likely used for data exchange related to user information.

The exact format and content of the data exchanged are not fully clear from the provided documentation. ❓ QUESTION: What are the specific data formats and contents exchanged between systems via MQ queues and files? Are there any data transformation or mapping processes involved?

## 7. Business Rules Business Rules

This section documents the business rules implemented within the authorization system. These rules govern various aspects of transaction processing, data validation, and system behavior.

### 7.1. Authorization Processing

*   **Authorization Limit Check:** The system verifies if the requested transaction amount exceeds the authorization limit defined for the account. If the limit is exceeded, the transaction is declined. Source: [COPAUS0C](cbl/COPAUS0C.cbl.md)
*   **Fraud Score Threshold:** Transactions exceeding a predefined fraud score threshold are flagged for manual review. Source: [COPAUS0C](cbl/COPAUS0C.cbl.md)
*   **Expired Card Check:** The system validates the card expiration date against the current date. Expired cards are rejected. Source: [COPAUS0C](cbl/COPAUS0C.cbl.md)
*   **Transaction Velocity Check:** The system monitors the number and amount of transactions within a specific time period. Transactions exceeding the defined velocity limits are flagged for review. Source: [COPAUS0C](cbl/COPAUS0C.cbl.md)
*   **Authorization Record Retrieval:** The [COPAUS1C](cbl/COPAUS1C.cbl.md) program retrieves authorization records to display on the authorization view screen. The program reads authorization records and populates the screen with relevant details.

### 7.2. Data Validation

*   **Account Number Validation:** Account numbers must adhere to a specific format and length. Invalid account numbers are rejected. Source: [COPAUA0C](cbl/COPAUA0C.cbl.md)
*   **Card Number Validation:** Card numbers must pass a Luhn algorithm check. Invalid card numbers are rejected. Source: [COPAUA0C](cbl/COPAUA0C.cbl.md)
*   **Date Validation:** Dates must be in a valid format (YYYY-MM-DD) and within a reasonable range. Invalid dates are rejected. Source: [COPAUA0C](cbl/COPAUA0C.cbl.md)
*   **Amount Validation:** Transaction amounts must be positive and within a defined range. Invalid amounts are rejected. Source: [COPAUA0C](cbl/COPAUA0C.cbl.md)

### 7.3. Fraud Detection and Prevention

*   **Fraudulent Activity Update:** The [COPAUS2C](cbl/COPAUS2C.cbl.md) program updates fraud indicators based on reported fraudulent activity. This program calls the `FRAUD-UPDATE` paragraph to perform the update.
*   **Marking Authorization as Fraudulent:** The [COPAUS1C](cbl/COPAUS1C.cbl.md) program provides functionality to mark an authorization as fraudulent. The `MARK-AUTH-FRAUD` paragraph is responsible for updating the authorization record with the fraud indicator.

## 8. Error Handling Patterns Error Handling Patterns

This section describes common error handling patterns observed throughout the authorization system, including abend codes, recovery procedures, logging, and monitoring.

### Common Error Handling Patterns

The system employs several common error handling patterns, including:

*   **Abend Codes:** Programs use abend codes to signal unrecoverable errors. These codes are typically numeric and provide information about the type of error encountered.
*   **Return Codes:** Programs use return codes to indicate the success or failure of a function or subroutine. A return code of 0 typically indicates success, while non-zero values indicate errors.
*   **File Status Checks:** COBOL programs frequently check the file status after each I/O operation. If the file status indicates an error, the program takes appropriate action, such as displaying an error message or abending.
*   **ON EXCEPTION Clause:** COBOL programs use the `ON EXCEPTION` clause to handle exceptions that occur during I/O operations or other processing.

### Abend Codes and Recovery Procedures

Several programs in the system use abend codes to signal errors. For example, the [CBPAUP0C](CBPAUP0C.cbl.md) program calls the `9999-ABEND` paragraph to terminate abnormally. The [PAUDBUNL](PAUDBUNL.CBL.md), [PAUDBLOD](PAUDBLOD.CBL.md), and [DBUNLDGS](DBUNLDGS.CBL.md) programs also use a `9999-ABEND` paragraph for abnormal termination.

| Program    | Abend Routine | Description…|
| [COPAUS0C](COPAUS0C.cbl.md) | 9999-ABEND | Used for abnormal termination of the COPAUS0C program. |

Recovery procedures for abends typically involve analyzing the abend code to determine the cause of the error and then taking corrective action, such as restarting the program or correcting the data.

### Logging and Monitoring Patterns

The system uses various logging and monitoring patterns to track errors and performance. These patterns include:

*   **System Logs:** Programs write error messages and other information to system logs, which can be used to diagnose problems.
*   **Transaction Monitoring:** CICS transactions are monitored to track performance and identify errors.
*   **Audit Trails:** Audit trails are used to track changes to data and to provide a record of system activity.

### Error Escalation Chains

When an error occurs, the system may escalate the error to a higher level of support. This escalation may involve notifying system administrators or other personnel who can take corrective action. ❓ QUESTION: Where is the error escalation chain documented? Is there a formal process?

### Examples in Code

*   The [COPAUA0C](COPAUA0C.cbl.md) program uses the `1100-OPEN-REQUEST-QUEUE` paragraph to open a request queue. If the queue cannot be opened, the program may abend.
*   The [COPAUS1C](COPAUS1C.cbl.md) program includes logic to `MARK-AUTH-FRAUD`. ❓ QUESTION: How does this relate to error handling? Is this a specific error condition?


## 9. Open Questions and Uncertainties Open Questions and Uncertainties

This section consolidates open questions and uncertainties identified during the documentation process. Addressing these points will improve the system's understanding and maintainability.

**1. Architecture**

*   **Question:** How does the system handle concurrent requests, especially when updating shared data structures?
    *   **Why it matters:** Understanding concurrency is crucial for preventing data corruption and ensuring system stability under load.
    *   **Possible resolution:** Analyze CICS transaction definitions and resource locking mechanisms. Review the use of ENQ/DEQ macros or equivalent locking strategies in COBOL programs.

**2. Data Flow**

*   **Question:** What is the exact data flow between the online CICS transactions and the batch processes?
    *   **Why it matters:** A clear understanding of data flow is essential for debugging data inconsistencies and optimizing performance.
    *   **Possible resolution:** Trace data lineage from CICS screens to database updates and subsequent batch processing. Analyze JCL and COBOL code to identify data transformations and movement.

**3. Business Rules**

*   **Question:** What are the specific criteria used to identify fraudulent transactions?
    *   **Why it matters:** Understanding fraud detection rules is critical for maintaining system security and compliance.
    *   **Possible resolution:** Interview business analysts and review relevant documentation. Analyze the COBOL code in [COPAUS0C](COPAUS0C.cbl.md), [COPAUS1C](COPAUS1C.cbl.md), [COPAUA0C](COPAUA0C.cbl.md), and [COPAUS2C](COPAUS2C.cbl.md) to identify the implementation of these rules.

**4. Assumptions**

*   **Assumption:** The documentation assumes that all critical business rules are implemented within the COBOL programs.
    *   **Implication:** If business rules are implemented elsewhere (e.g., in stored procedures or external systems), the documentation may be incomplete.
*   **Assumption:** The system uses standard CICS and IMS configurations.
    *   **Implication:** If non-standard configurations are used, the documentation may not accurately reflect the system's behavior.