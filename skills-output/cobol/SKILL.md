---
name: cobol
description: COBOL program documentation
---

# COBOL Documentation

| Program | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0C | This COBOL batch IMS program, CBPAUP0C, deletes expired pending authorization messages from the IMS database. It reads pending authorization summary and detail segments, checks if the... | [Full docs](../documentation/cbl/CBPAUP0C.cbl.md) |
| COPAUA0C | The COPAUA0C program processes authorization requests from a message queue, validates the requests, and updates relevant systems. It retrieves messages from a request queue, processes the... | [Full docs](../documentation/cbl/COPAUA0C.cbl.md) |
| COPAUS0C | COPAUS0C is a CICS program that displays pending authorization summary information for a given account. It retrieves authorization details from an IMS database and presents them on a CICS screen,... | [Full docs](../documentation/cbl/COPAUS0C.cbl.md) |
| COPAUS1C | The COPAUS1C program is a CICS transaction that displays authorization details and allows users to mark authorizations as fraudulent. It retrieves authorization records based on account ID and... | [Full docs](../documentation/cbl/COPAUS1C.cbl.md) |
| COPAUS2C | This COBOL program processes fraud reports, inserts them into the CARDDEMO.AUTHFRDS table, and updates existing records if a duplicate is found. It retrieves the current date and time, formats the... | [Full docs](../documentation/cbl/COPAUS2C.cbl.md) |
| DBUNLDGS | The COBOL program DBUNLDGS unloads data from an IMS database related to pending authorizations. It reads pending authorization summary segments (root) and writes them to a sequential output file.... | [Full docs](../documentation/cbl/DBUNLDGS.CBL.md) |
| PAUDBLOD | The COBOL program PAUDBLOD reads two sequential input files (INFILE1 and INFILE2), treats the records as IMS database segments (root and child), and inserts them into an IMS database. It reads... | [Full docs](../documentation/cbl/PAUDBLOD.CBL.md) |
| PAUDBUNL | The COBOL program PAUDBUNL extracts pending authorization summary and detail records from an IMS database and writes them to sequential output files. It reads authorization summary records and... | [Full docs](../documentation/cbl/PAUDBUNL.CBL.md) |
