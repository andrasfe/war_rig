---
name: cobol
description: COBOL program documentation
---

# COBOL Documentation

| Program | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0C | The CBPAUP0C program purges expired authorization records from the IMS PAUT database. It reads authorization summary and detail segments, determines if authorizations have expired based on a... | [Full docs](../documentation/cbl/CBPAUP0C.cbl.md) |
| COPAUA0C | COPAUA0C is a CICS COBOL IMS MQ program that functions as a card authorization decision program. It likely receives authorization requests via MQ, processes them, and makes authorization... | [Full docs](../documentation/cbl/COPAUA0C.cbl.md) |
| COPAUS0C | This CICS COBOL program, COPAUS0C, retrieves and displays pending authorization summaries for a given account ID. It allows users to navigate through multiple pages of authorization details and... | [Full docs](../documentation/cbl/COPAUS0C.cbl.md) |
| COPAUS1C | The COPAUS1C program is a CICS transaction that displays authorization details for a given account and authorization key. It allows users to mark authorizations as fraudulent, navigate through... | [Full docs](../documentation/cbl/COPAUS1C.cbl.md) |
| COPAUS2C | The COPAUS2C program is a CICS COBOL DB2 program that marks authorization messages as fraudulent in the CARDDEMO.AUTHFRDS table. It receives authorization data via the CICS COMMAREA, inserts a new... | [Full docs](../documentation/cbl/COPAUS2C.cbl.md) |
| DBUNLDGS | The COBOL program DBUNLDGS extracts data from an IMS database related to pending authorizations and writes it to sequential output files. It reads pending authorization summary segments (root) and... | [Full docs](../documentation/cbl/DBUNLDGS.CBL.md) |
| PAUDBLOD | The COBOL program PAUDBLOD reads root segment records from INFILE1 and child segment records from INFILE2, then inserts them into an IMS database. It uses CBLTDLI calls to insert the root and... | [Full docs](../documentation/cbl/PAUDBLOD.CBL.md) |
| PAUDBUNL | The COBOL program PAUDBUNL extracts authorization summary and detail records from an IMS database and writes them to two sequential output files. It reads authorization summary records and then... | [Full docs](../documentation/cbl/PAUDBUNL.CBL.md) |
