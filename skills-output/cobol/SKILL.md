---
name: cobol
description: COBOL program documentation
---

# COBOL Documentation

| Program | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0C | This batch COBOL IMS program, CBPAUP0C, deletes expired pending authorization messages from the IMS database. It reads pending authorization summary and detail segments, checks if the... | [Full docs](../documentation/cbl/CBPAUP0C.cbl.md) |
| COPAUA0C | This is an empty COBOL program. It does not contain any code or logic. | [Full docs](../documentation/cbl/COPAUA0C.cbl.md) |
| COPAUS0C | This CICS program displays a summary of authorizations for a given account. It retrieves authorization details from an IMS database and presents them on a screen, allowing the user to page through... | [Full docs](../documentation/cbl/COPAUS0C.cbl.md) |
| COPAUS1C | COPAUS1C is a CICS program that displays authorization details and allows users to mark authorizations as fraudulent. It retrieves authorization information based on account ID and authorization... | [Full docs](../documentation/cbl/COPAUS1C.cbl.md) |
| COPAUS2C | The COPAUS2C program is a CICS COBOL DB2 program that marks an authorization message as fraudulent. It inserts a record into the CARDDEMO.AUTHFRDS table, and if a duplicate record exists, it... | [Full docs](../documentation/cbl/COPAUS2C.cbl.md) |
| DBUNLDGS | The COBOL program DBUNLDGS retrieves pending authorization summary and detail segments from an IMS database and writes them to output files. It uses the CBLTDLI call to interface with IMS and... | [Full docs](../documentation/cbl/DBUNLDGS.CBL.md) |
| PAUDBLOD | The COBOL program PAUDBLOD reads root and child segment files (INFILE1 and INFILE2), and inserts them into an IMS database. It reads PENDING-AUTH-SUMMARY records from INFILE1 and inserts them as... | [Full docs](../documentation/cbl/PAUDBLOD.CBL.md) |
| PAUDBUNL | The PAUDBUNL program extracts authorization summary and detail information from an IMS database and writes it to two sequential output files. It reads authorization summary records and then... | [Full docs](../documentation/cbl/PAUDBUNL.CBL.md) |
