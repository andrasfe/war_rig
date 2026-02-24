---
name: cobol
description: COBOL program documentation
---

# COBOL Documentation

| Program | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0C | This batch COBOL IMS program purges expired pending authorization messages from an IMS database. It sequentially reads root summary segments (PAUTSUM0) and their child detail segments (PAUTDTL1),... | [Full docs](../documentation/cbl/CBPAUP0C.cbl.md) |
| COPAUA0C | This CICS COBOL program processes credit card authorization requests received via MQ queue triggered by CICS transaction. It parses the request, looks up card cross-reference, account and customer... | [Full docs](../documentation/cbl/COPAUA0C.cbl.md) |
| COPAUS0C | COPAUS0C is a CICS online program that provides a paginated summary view of pending authorization messages for a specified account ID in the CardDemo authorization module. It retrieves account,... | [Full docs](../documentation/cbl/COPAUS0C.cbl.md) |
| COPAUS1C | COPAUS1C is a CICS transaction program that provides a detailed view of pending authorization records from an IMS database. It retrieves and displays authorization details on a BMS screen... | [Full docs](../documentation/cbl/COPAUS1C.cbl.md) |
| COPAUS2C | COPAUS2C is a CICS COBOL program in the CardDemo authorization module that marks an authorization message as fraud by inserting a new record into the DB2 AUTHFRDS table or updating an existing one... | [Full docs](../documentation/cbl/COPAUS2C.cbl.md) |
| DBUNLDGS | DBUNLDGS is an IMS DL/I batch utility program that unloads pending authorization summary (root) and details (child) segments from the PAUT IMS database using GN and GNP calls on PAUTBPCB. It... | [Full docs](../documentation/cbl/DBUNLDGS.CBL.md) |
| PAUDBLOD | PAUDBLOD is a batch utility program that loads pending authorization data into an IMS hierarchical database. It reads sequential root segment records from INFILE1 and inserts them as PAUTSUM0... | [Full docs](../documentation/cbl/PAUDBLOD.CBL.md) |
| PAUDBUNL | PAUDBUNL is a batch IMS DL/I program that reads root authorization summary segments from an IMS database using GN calls and their child detail segments using GNP calls, writing summary records to... | [Full docs](../documentation/cbl/PAUDBUNL.CBL.md) |
