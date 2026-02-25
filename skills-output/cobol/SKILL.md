---
name: cobol
description: COBOL program documentation
---

# COBOL Documentation

| Program | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0C | CBPAUP0C is a batch COBOL IMS DL/I program that purges expired pending authorization detail segments from the authorization database and deletes their parent summary segments if no remaining... | [Full docs](../documentation/cbl/CBPAUP0C.cbl.md) |
| COPAUA0C | COPAUA0C is a CICS COBOL program that processes authorization requests from an IBM MQ request queue in a loop until a processing limit is reached or no more messages are available. It initializes... | [Full docs](../documentation/cbl/COPAUA0C.cbl.md) |
| COPAUS0C | COPAUS0C is a CICS transaction program (CPVS) that displays a paginated summary list of up to 5 pending authorization transactions for a specified account ID using BMS map COPAU0A. It supports... | [Full docs](../documentation/cbl/COPAUS0C.cbl.md) |
| COPAUS1C | This CICS program displays detailed view of a selected pending authorization record from IMS database for a given account. It supports user interactions to validate selection, navigate to next... | [Full docs](../documentation/cbl/COPAUS1C.cbl.md) |
| COPAUS2C | COPAUS2C is a CICS online program that processes authorization fraud data by formatting the current date and time, parsing authorization timestamps, moving input authorization details from the... | [Full docs](../documentation/cbl/COPAUS2C.cbl.md) |
| DBUNLDGS | DBUNLDGS is an IMS batch utility that unloads pending authorization summary (root) and detail (child) segments from the hierarchical IMS database PAUT via sequential GN/GNP calls on PAUTBPCB. It... | [Full docs](../documentation/cbl/DBUNLDGS.CBL.md) |
| PAUDBLOD | This program is a batch utility designed to load data into an IMS database from two sequential input files. It performs insertions for both root segments (PAUTSUM0) and child segments (PAUTDTL1)... | [Full docs](../documentation/cbl/PAUDBLOD.CBL.md) |
| PAUDBUNL | PAUDBUNL is an IMS DL/I unload utility that retrieves all root segments (PAUTSUM0 Pending Authorization Summary) using unqualified GN calls and their dependent child segments (PAUTDTL1 Pending... | [Full docs](../documentation/cbl/PAUDBUNL.CBL.md) |
