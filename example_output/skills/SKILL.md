---
name: system-overview
description: System documentation overview
---

# System Overview

This system, driven by the TicketOrchestrator, manages and processes authorization requests, primarily focusing on financial transactions and fraud detection. Its mission is to ensure the validity and security of authorizations by validating data against various internal and external sources. The system caters to internal users responsible for authorization processing, fraud investigation, and system maintenance, as well as external customers whose transactions are being authorized. The core functionality revolves around receiving authorization requests, validating them against customer, account, and cross-reference data, making authorization decisions, and updating the authorization database. The system also provides online screens for viewing and managing authorization details.

The system's major capabilities include receiving authorization requests via MQSeries queues, extracting relevant data from these requests, and enriching the data by retrieving information from IMS databases and VSAM files. Key workflows involve processing authorization requests, validating data against customer, account, and cross-reference information, making authorization decisions based on predefined rules, and updating the authorization database with the results. The system also supports online inquiry and maintenance functions through CICS transactions, allowing users to view and update authorization details. The system uses batch jobs to unload and load IMS databases.

The system is built on a foundation of COBOL programs interacting with IMS databases, VSAM files, and MQSeries queues. JCL is used to orchestrate batch processes for data loading and unloading. COBOL programs use embedded SQL (CBLTDLI) to access IMS databases. CICS transactions provide online access to authorization data. The system leverages copybooks to ensure data consistency across different programs. The system integrates with external systems through MQSeries for receiving authorization requests and sending responses. It also interacts with IMS databases for storing and retrieving authorization data, and VSAM files for accessing customer, account, and cross-reference information.


## Categories

- [BMS](bms/SKILL.md) - BMS map documentation (screen definitions) (2 files)
- [BMS-COPYBOOK](bms-copybook/SKILL.md) - BMS copybook documentation (screen mappings) (2 files)
- [COBOL](cobol/SKILL.md) - COBOL program documentation (8 files)
- [COPYBOOK](copybook/SKILL.md) - Copybook documentation (shared data structures) (9 files)
- [DDL](ddl/SKILL.md) - DDL documentation (database definitions) (2 files)
- [IMS](ims/SKILL.md) - IMS documentation (database/PSB definitions) (8 files)
- [JCL](jcl/SKILL.md) - JCL job documentation (5 files)
