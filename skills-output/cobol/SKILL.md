---
name: cobol
description: COBOL program documentation
---

# COBOL Documentation

| Program | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0C | CBPAUP0C is a batch COBOL IMS program that deletes expired pending authorization messages from the IMS database. It reads pending authorization summary and detail segments, checks if the... | [Full docs](../documentation/cbl/CBPAUP0C.cbl.md) |
| COPAUA0C | COPAUA0C is a CICS COBOL program that processes authorization requests from a message queue, validates them, and updates relevant IMS databases. It retrieves messages from a request queue,... | [Full docs](../documentation/cbl/COPAUA0C.cbl.md) |
| COPAUS0C | This is an empty COBOL program. It does not perform any operations or contain any logic. | [Full docs](../documentation/cbl/COPAUS0C.cbl.md) |
| COPAUS1C | This CICS COBOL program, part of the CardDemo application's Authorization Module, provides a detailed view of an authorization message. It receives input from a BMS map, processes it, and displays... | [Full docs](../documentation/cbl/COPAUS1C.cbl.md) |
| COPAUS2C | COPAUS2C is a CICS COBOL DB2 program that marks an authorization message as fraudulent. It inserts a record into the CARDDEMO.AUTHFRDS table with fraud information, and if a duplicate record... | [Full docs](../documentation/cbl/COPAUS2C.cbl.md) |
| DBUNLDGS | The COBOL program DBUNLDGS is designed to unload data from an IMS database related to pending authorizations. It retrieves pending authorization summary segments (root) and detail segments (child)... | [Full docs](../documentation/cbl/DBUNLDGS.CBL.md) |
| PAUDBLOD | The COBOL program PAUDBLOD reads root and child segment files (INFILE1 and INFILE2), and inserts them into an IMS database. It reads PENDING-AUTH-SUMMARY records from INFILE1 and inserts them as... | [Full docs](../documentation/cbl/PAUDBLOD.CBL.md) |
| PAUDBUNL | This COBOL program extracts authorization summary and detail information from an IMS database and writes it to two sequential output files. It reads authorization summary records and then reads... | [Full docs](../documentation/cbl/PAUDBUNL.CBL.md) |
