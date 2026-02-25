---
name: copybook
description: Copybook documentation (shared data structures)
---

# COPYBOOK Documentation

| Name | Description | Documentation |
|---------|-------------|---------------|
| CCPAUERY | This copybook defines the ERROR-LOG-RECORD structure for logging errors in pending authorization processes. It includes fields for timestamp (ERR-DATE, ERR-TIME), identifiers (ERR-APPLICATION,... | [Full docs](../documentation/cpy/CCPAUERY.cpy.md) |
| CCPAURLY | This COBOL copybook defines a data structure for Pending Authorization Response records, including fields for card number, transaction ID, authorization ID code, response code, response reason,... | [Full docs](../documentation/cpy/CCPAURLY.cpy.md) |
| CCPAURQY | This COBOL copybook defines a data structure at the 05 group level for a 'PENDING AUTHORIZATION REQUEST' used in payment processing systems. It specifies 18 elementary fields including timestamps... | [Full docs](../documentation/cpy/CCPAURQY.cpy.md) |
| CIPAUDTY | This copybook defines the IMS segment layout for Pending Authorization Details (CIPAUDTY). It structures fields for authorization keys, timestamps, card details, merchant information, transaction... | [Full docs](../documentation/cpy/CIPAUDTY.cpy.md) |
| CIPAUSMY | This copybook defines the data structure for the IMS segment named PENDING AUTHORIZATION SUMMARY. It specifies fields for account identification, customer ID, authorization and account statuses,... | [Full docs](../documentation/cpy/CIPAUSMY.cpy.md) |
| IMSFUNCS | This copybook defines a data structure FUNC-CODES containing standard IMS DL/I function codes used to specify database operations in CALL statements to IMS PCBs, such as 'GU  ' for Get Unique,... | [Full docs](../documentation/cpy/IMSFUNCS.cpy.md) |
| PADFLPCB | This COBOL copybook defines the PADFLPCB level-01 group item, which maps the standard IMS Database Program Control Block (DBPCB) structure. It provides named fields for accessing IMS-provided PCB... | [Full docs](../documentation/cpy/PADFLPCB.CPY.md) |
| PASFLPCB | This COBOL copybook defines the PASFLPCB 01-level record layout, which represents the Program Communication Block (PCB) for the IMS DL/I database named PASFL. It includes fields for database name,... | [Full docs](../documentation/cpy/PASFLPCB.CPY.md) |
| PAUTBPCB | This copybook defines the IMS Program Communication Block (PCB) structure PAUTBPCB used for DL/I database access to the PAUTB database. It includes fields for DBD name, segment level, PCB status,... | [Full docs](../documentation/cpy/PAUTBPCB.CPY.md) |
