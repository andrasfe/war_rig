---
name: cobol
description: COBOL program documentation
---

# COBOL Documentation

| Program | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0C | The CBPAUP0C program is a batch COBOL IMS program that deletes expired pending authorization messages from the IMS database. It reads pending authorization summary and detail segments, checks if... | [Full docs](../documentation/cbl/CBPAUP0C.cbl.md) |
| COPAUA0C | This CICS COBOL program, COPAUA0C, is a card authorization decision program. It retrieves authorization requests from a MQ queue, processes them, and makes an authorization decision. The program... | [Full docs](../documentation/cbl/COPAUA0C.cbl.md) |
| COPAUS0C | COPAUS0C is a CICS program that displays pending authorization summaries for a given account. It retrieves authorization details from an IMS database and presents them on a CICS screen, allowing... | [Full docs](../documentation/cbl/COPAUS0C.cbl.md) |
| COPAUS1C | The COPAUS1C program is a CICS transaction that displays authorization details and allows users to mark authorizations as fraudulent. It retrieves authorization information from an IMS database... | [Full docs](../documentation/cbl/COPAUS1C.cbl.md) |
| COPAUS2C | This COBOL program processes fraud reports and updates the CARDDEMO.AUTHFRDS table in a DB2 database. It inserts new records or updates existing ones based on the SQLCODE returned from the INSERT... | [Full docs](../documentation/cbl/COPAUS2C.cbl.md) |
| DBUNLDGS | The COBOL program DBUNLDGS unloads data from an IMS database related to pending authorizations. It reads pending authorization summary segments (root) and detail segments (child) from the IMS... | [Full docs](../documentation/cbl/DBUNLDGS.CBL.md) |
| PAUDBLOD | This COBOL program reads root segment records from INFILE1 and child segment records from INFILE2, then inserts them into an IMS database. It uses CBLTDLI calls to perform the database insertions,... | [Full docs](../documentation/cbl/PAUDBLOD.CBL.md) |
| PAUDBUNL | The COBOL program PAUDBUNL unloads data from an IMS database related to pending authorizations. It reads pending authorization summary segments (root) and pending authorization details segments... | [Full docs](../documentation/cbl/PAUDBUNL.CBL.md) |
