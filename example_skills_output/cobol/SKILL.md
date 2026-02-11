---
name: cobol
description: COBOL program documentation
---

# COBOL Documentation

| Program | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0C | This program CBPAUP0C purges expired authorization details from the IMS database. It reads authorization summary and detail segments, checks if the authorization is expired based on a configurable... | [Full docs](../documentation/cbl/CBPAUP0C.cbl.md) |
| COPAUA0C | This CICS COBOL program, COPAUA0C, is a card authorization decision program that processes authorization requests, validates card and customer data, makes an authorization decision, and updates... | [Full docs](../documentation/cbl/COPAUA0C.cbl.md) |
| COPAUS0C | COPAUS0C is a CICS program that displays pending authorization details for a given account. It retrieves authorization summaries and details from an IMS database and presents them on a CICS... | [Full docs](../documentation/cbl/COPAUS0C.cbl.md) |
| COPAUS1C | The COPAUS1C program is a CICS transaction that displays authorization details and allows users to mark authorizations as fraudulent. It receives input from a calling program via the COMMAREA,... | [Full docs](../documentation/cbl/COPAUS1C.cbl.md) |
| COPAUS2C | This CICS COBOL program, COPAUS2C, marks an authorization message as fraudulent by inserting a record into the CARDDEMO.AUTHFRDS table. If a record already exists, it updates the existing record... | [Full docs](../documentation/cbl/COPAUS2C.cbl.md) |
| DBUNLDGS | The COBOL program DBUNLDGS extracts pending authorization summary and detail records from an IMS database and writes them to sequential files. It reads pending authorization summary records (root... | [Full docs](../documentation/cbl/DBUNLDGS.CBL.md) |
| PAUDBLOD | This COBOL program reads root segment records from INFILE1 and child segment records from INFILE2, then inserts them into an IMS database. It reads each file until the end-of-file is reached,... | [Full docs](../documentation/cbl/PAUDBLOD.CBL.md) |
| PAUDBUNL | This COBOL program, named PAUDBUNL, unloads data from an IMS database related to pending authorizations and writes it to two sequential output files. It reads pending authorization summary... | [Full docs](../documentation/cbl/PAUDBUNL.CBL.md) |
