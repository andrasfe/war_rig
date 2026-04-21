# PAUTBPCB

**File**: `cpy/PAUTBPCB.CPY`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-04-21 13:43:42.249684

## Purpose

This copybook defines the data structure for a PCB (Program Communication Block) used in IMS (Information Management System) database interaction. It contains fields related to database name, segment level, processing options, segment name, key feedback, and number of sensitive segments.

**Business Context**: This copybook is used in IMS database applications to access and manipulate data within IMS databases. It provides a structured way to interact with IMS segments.

## Paragraphs/Procedures

### PAUTBPCB Definition
This section defines the PAUTBPCB data structure, which represents a Program Communication Block (PCB) used in IMS database interactions. The PCB provides information about the database, segment, and processing options. It includes fields such as the database name (PAUT-DBDNAME), segment level (PAUT-SEG-LEVEL), PCB status (PAUT-PCB-STATUS), processing options (PAUT-PCB-PROCOPT), segment name (PAUT-SEG-NAME), key feedback name (PAUT-KEYFB-NAME), number of sensitive segments (PAUT-NUM-SENSEGS), and key feedback (PAUT-KEYFB). This structure is used to access and manipulate data within IMS databases by providing a structured way to interact with IMS segments. The copybook does not perform any error handling or call any other programs; it only defines the data structure.
