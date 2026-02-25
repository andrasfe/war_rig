---
name: system-overview
description: System documentation overview
---

# System Overview

The Pending Authorization (PAU) System is a mission-critical financial application designed to manage held transaction authorizations, preventing overdrafts and mitigating fraud risks in real-time banking operations. It addresses the core business problem of processing pending authorization requests from ATMs, point-of-sale terminals, and online channels that temporarily exceed account limits or trigger fraud alerts. By holding suspicious transactions in IMS databases until manual review or automated approval, the system ensures compliance with regulatory limits while minimizing false declines. Primary users include bank tellers accessing CICS terminals for inquiry and override decisions, back-office operators running batch jobs for end-of-day reconciliation, and automated schedulers handling data unload/load cycles.

Key functional capabilities revolve around batch processing of pending authorizations via [CBPAUP0C](cbl/CBPAUP0C.cbl.md), which reads summary and detail files like PAUTSUM0 and PAUTDTL1 before updating IMS AUTHDB with approved counts. Online workflows in [COPAUS0C](cbl/COPAUS0C.cbl.md)—the system's central hub with 59 relationships—support screen-based inquiries and updates using BMS maps [COPAU00](bms/COPAU00.bms.md) and [COPAU01](bms/COPAU01.bms.md), reading datasets like PENDING-AUTH-DETAILS and writing outputs such as PAULST-SCREEN. [COPAUS1C](cbl/COPAUS1C.cbl.md) extends this with additional transaction handling, calling back into [COPAUS0C](cbl/COPAUS0C.cbl.md) and integrating with external fraud services like WS-PGM-AUTH-FRAUD. Data preparation workflows use [PAUDBUNL](cbl/PAUDBUNL.CBL.md) for IMS unloading to flat files, supporting batch jobs like [DBPAUTP0](jcl/DBPAUTP0.jcl.md) that read 36 datasets including DDPAUTX0. [COPAUA0C](cbl/COPAUA0C.cbl.md) handles asynchronous messaging via IBM MQ calls (MQOPEN, MQGET, MQPUT1, MQCLOSE), processing MQ queues for decoupled auth notifications. Core workflows include entry-point JCL jobs like [UNLDPADB](jcl/UNLDPADB.JCL.md) for database maintenance and [CBPAUP0J](jcl/CBPAUP0J.jcl.md) invoking [CBPAUP0C](cbl/CBPAUP0C.cbl.md), forming a complete cycle from data ingestion to approval.

The technical foundation is rooted in COBOL for robust business logic across 8 documented programs, leveraging IMS DL/I for hierarchical database access via PSBs like PSBPAUTB and DBDs such as PADFLDBD and PASFLDBD. CICS transaction processing drives online components, evident in DFH* copybooks (DFHAID, DFHBMSCA) and COMMAREA handling in [COPAUS0C](cbl/COPAUS0C.cbl.md). JCL batch orchestration manages 5 key jobs, including data load/unload with utilities like DFSRRC00. Shared copybooks like [CIPAUDTY](cpy/CIPAUDTY.cpy.md) (used by 8 programs) and [CIPAUSMY](cpy/CIPAUSMY.cpy.md) (used by 7) define common auth structures, while IMSFUNCS provides reusable DL/I calls. MQSeries enables event-driven integration, and assembler-level IMS access appears in utilities like [PAUDBLOD](cbl/PAUDBLOD.CBL.md).


## Categories

- [BMS](bms/SKILL.md) - BMS map documentation (screen definitions) (2 files)
- [BMS-COPYBOOK](bms-copybook/SKILL.md) - BMS copybook documentation (screen mappings) (2 files)
- [COBOL](cobol/SKILL.md) - COBOL program documentation (8 files)
- [COPYBOOK](copybook/SKILL.md) - Copybook documentation (shared data structures) (9 files)
- [DDL](ddl/SKILL.md) - DDL documentation (database definitions) (2 files)
- [IMS](ims/SKILL.md) - IMS documentation (database/PSB definitions) (8 files)
- [JCL](jcl/SKILL.md) - JCL job documentation (5 files)
- [SKILLS](skills/SKILL.md) - SKILLS documentation (1 files)
