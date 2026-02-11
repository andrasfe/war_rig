---
name: system-overview
description: System documentation overview
---

# System Overview

This document describes the architecture of a system centered around authorization processing, likely within a financial or transaction-based context. The system's primary purpose is to manage and process authorization requests, maintain authorization details, and potentially flag fraudulent activities. It serves users involved in authorization management, fraud detection, and customer service. The system aims to ensure secure and valid transactions by verifying authorization details against various data sources. Key stakeholders include fraud analysts, customer service representatives, and IT operations personnel.

The system's major capabilities include receiving authorization requests, validating these requests against customer account and profile data, making decisions based on predefined business rules, and updating authorization records in a database. It handles transactions related to authorization creation, modification, deletion, and fraud flagging. Key workflows involve retrieving authorization details, cross-referencing account information, and processing authorization decisions. The system also includes batch processes for unloading and loading data related to authorization and related entities. The online components likely provide screens for viewing and managing authorization data.

The system is built using a combination of mainframe technologies, including COBOL for business logic, JCL for batch processing, IMS for database management, and potentially CICS for online transaction processing. COBOL programs interact with IMS databases using CBLTDLI calls. JCL jobs automate data unloading and loading processes. Copybooks define data structures shared across multiple programs. The system uses MQSeries for message queuing, enabling asynchronous communication between components.


## Categories

- [BMS](bms/SKILL.md) - BMS map documentation (screen definitions) (2 files)
- [BMS-COPYBOOK](bms-copybook/SKILL.md) - BMS copybook documentation (screen mappings) (2 files)
- [COBOL](cobol/SKILL.md) - COBOL program documentation (8 files)
- [COPYBOOK](copybook/SKILL.md) - Copybook documentation (shared data structures) (9 files)
- [DDL](ddl/SKILL.md) - DDL documentation (database definitions) (2 files)
- [IMS](ims/SKILL.md) - IMS documentation (database/PSB definitions) (8 files)
- [JCL](jcl/SKILL.md) - JCL job documentation (5 files)
