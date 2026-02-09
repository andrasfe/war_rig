---
name: cobol
description: COBOL program documentation
---

# COBOL Documentation

| Program | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0C | This COBOL batch IMS program, CBPAUP0C, deletes expired pending authorization messages from the IMS database. It reads pending authorization summary and detail segments, checks if the... | [Full docs](../documentation/cbl/CBPAUP0C.cbl.md) |
| COPAUA0C | This COBOL program processes authorization requests, reads card, account, and customer information, makes a decision on whether to approve or decline the authorization, sends a response, and... | [Full docs](../documentation/cbl/COPAUA0C.cbl.md) |
| COPAUS0C | This COBOL program is a CICS transaction that retrieves and displays account, customer, and authorization summary information based on an account ID. It interacts with VSAM files for card,... | [Full docs](../documentation/cbl/COPAUS0C.cbl.md) |
| COPAUS1C | The COPAUS1C program is a CICS transaction that displays authorization details and allows users to mark authorizations as fraudulent. It receives input from a calling program (CDEMO), retrieves... | [Full docs](../documentation/cbl/COPAUS1C.cbl.md) |
| COPAUS2C | This CICS COBOL program, COPAUS2C, marks authorization messages as fraudulent in the CARDDEMO.AUTHFRDS DB2 table. It receives transaction details via the CICS COMMAREA, formats the data, and... | [Full docs](../documentation/cbl/COPAUS2C.cbl.md) |
| DBUNLDGS | The COBOL program DBUNLDGS unloads data from an IMS database related to pending authorizations and writes it to GSAM datasets. It retrieves pending authorization summary records and their... | [Full docs](../documentation/cbl/DBUNLDGS.CBL.md) |
| PAUDBLOD | The PAUDBLOD program reads two sequential files, INFILE1 and INFILE2, and loads data into an IMS database. INFILE1 contains root segment data for 'PAUTSUM0' and INFILE2 contains child segment data... | [Full docs](../documentation/cbl/PAUDBLOD.CBL.md) |
| PAUDBUNL | This COBOL program, named PAUDBUNL, unloads data from an IMS database related to pending authorizations and writes it to two sequential output files. It reads pending authorization summary... | [Full docs](../documentation/cbl/PAUDBUNL.CBL.md) |
