---
name: system-overview
description: CardDemo System documentation overview
---

# CardDemo System Overview

This system, driven by the TicketOrchestrator, revolves around the management and processing of authorization data, likely related to financial transactions or access control. Its primary mission is to efficiently handle authorization requests, validate them against various data sources, and persist the results for auditing and reporting. The system caters to internal users who need to manage and analyze authorization data, as well as external systems that rely on the authorization decisions made by this system. The core functionality involves receiving authorization requests, enriching them with data from customer, account, and cross-reference databases, applying business rules to make authorization decisions, and then recording these decisions in a database.

The system's major capabilities include processing authorization requests from an MQ queue, retrieving related data from IMS databases and sequential files, applying complex business rules to determine approval or denial, and generating response messages. Key workflows involve reading authorization requests from [MQ Series](https://www.ibm.com/products/mq), accessing customer and account information, validating authorization rules, and updating authorization summaries. The system also includes batch processes for unloading and loading IMS databases, ensuring data integrity and availability. The online CICS components provide interactive screens for viewing and managing authorization data.

The technical foundation of this system is built upon a combination of COBOL programs, JCL scripts, IMS databases, and MQ Series messaging. COBOL programs handle the core business logic and data manipulation, while JCL scripts automate batch processes for data loading and unloading. IMS databases store critical authorization data, and MQ Series provides a reliable messaging infrastructure for receiving authorization requests and sending responses. The system leverages [CBLTDLI](https://www.ibm.com/docs/en/ims/15?topic=interface-cbltdli-call-interface) calls to interact with the IMS databases. The [DFSRRC00](https://www.ibm.com/docs/en/ims/15?topic=utilities-dfsrrc00) utility is used for database operations within the JCL jobs.


## Categories

- [BMS](bms/SKILL.md) - BMS map documentation (screen definitions) (2 files)
- [BMS-COPYBOOK](bms-copybook/SKILL.md) - BMS copybook documentation (screen mappings) (2 files)
- [COBOL](cobol/SKILL.md) - COBOL program documentation (8 files)
- [COPYBOOK](copybook/SKILL.md) - Copybook documentation (shared data structures) (9 files)
- [DDL](ddl/SKILL.md) - DDL documentation (database definitions) (2 files)
- [IMS](ims/SKILL.md) - IMS documentation (database/PSB definitions) (8 files)
- [JCL](jcl/SKILL.md) - JCL job documentation (5 files)
