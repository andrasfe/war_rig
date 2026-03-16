---
name: cobol
description: COBOL program documentation
---

# COBOL Documentation

| Program | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0C | This batch COBOL IMS program deletes expired pending authorization messages from the IMS database. It reads pending authorization summary and detail segments, checks if the authorization has... | [Full docs](../documentation/cbl/CBPAUP0C.cbl.md) |
| COPAUA0C | This CICS COBOL program processes authorization requests received from an MQ queue, schedules an IMS PSB, processes the authorization, and then terminates. It retrieves the queue name and trigger... | [Full docs](../documentation/cbl/COPAUA0C.cbl.md) |
| COPAUS0C | COPAUS0C is a CICS program that displays pending authorization summaries for a given account ID. It retrieves authorization details from an IMS database and presents them on a screen, allowing the... | [Full docs](../documentation/cbl/COPAUS0C.cbl.md) |
| COPAUS1C | The COPAUS1C program is a CICS transaction that displays authorization details for a given account and authorization key. It allows users to mark authorizations as fraudulent and navigate through... | [Full docs](../documentation/cbl/COPAUS1C.cbl.md) |
| COPAUS2C | COPAUS2C is a CICS COBOL DB2 program that marks authorization messages as fraudulent. It inserts a record into the CARDDEMO.AUTHFRDS table or updates an existing record if a duplicate is found,... | [Full docs](../documentation/cbl/COPAUS2C.cbl.md) |
| DBUNLDGS | The COBOL program DBUNLDGS extracts data from an IMS database related to pending authorizations and writes it to sequential output files. It reads pending authorization summary segments (root) and... | [Full docs](../documentation/cbl/DBUNLDGS.CBL.md) |
| PAUDBLOD | The COBOL program PAUDBLOD reads root segment records from INFILE1 and child segment records from INFILE2, then inserts them into an IMS database. It uses CBLTDLI calls to insert the root and... | [Full docs](../documentation/cbl/PAUDBLOD.CBL.md) |
| PAUDBUNL | The PAUDBUNL program unloads pending authorization summary and detail segments from an IMS database to two sequential output files. It reads PAUTSUM0 root segments and PAUTDTL1 child segments from... | [Full docs](../documentation/cbl/PAUDBUNL.CBL.md) |
