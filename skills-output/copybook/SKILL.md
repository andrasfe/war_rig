---
name: copybook
description: Copybook documentation (shared data structures)
---

# COPYBOOK Documentation

| Name | Description | Documentation |
|---------|-------------|---------------|
| CCPAUERY | This COBOL copybook defines the ERROR-LOG-RECORD data structure for logging pending authorization errors. It standardizes fields for timestamp, application details, error severity levels with... | [Full docs](../documentation/cpy/CCPAUERY.cpy.md) |
| CCPAURLY | Copybook defining the data structure for Pending Authorization Response (PA-RL). Includes fields for card number, transaction ID, authorization ID code, response code, response reason, and... | [Full docs](../documentation/cpy/CCPAURLY.cpy.md) |
| CCPAURQY | This COBOL copybook defines a level 05 data group structure for a Pending Authorization Request, including fields for auth date/time (lines 19-20), card number/expiry/auth type (lines 21-23),... | [Full docs](../documentation/cpy/CCPAURQY.cpy.md) |
| CIPAUDTY | This COPYBOOK defines the data layout for the IMS database segment 'PENDING AUTHORIZATION DETAILS' (line 2). It includes a group item PA-AUTHORIZATION-KEY at level 05 containing only two packed... | [Full docs](../documentation/cpy/CIPAUDTY.cpy.md) |
| CIPAUSMY | This copybook defines the layout for the IMS 'Pending Authorization Summary' segment. Key fields include account and customer identifiers, authorization and account statuses, credit/cash limits... | [Full docs](../documentation/cpy/CIPAUSMY.cpy.md) |
| IMSFUNCS | COBOL copybook defining IMS DL/I function codes as fixed 4-byte constants and a PARMCOUNT field for IMS database call standardization. Includes codes for retrieve (GU, GHU, GN, GHN, GNP, GHNP),... | [Full docs](../documentation/cpy/IMSFUNCS.cpy.md) |
| PADFLPCB | This COBOL copybook defines the 01 PADFLPCB record structure, which is the IMS Database Program Control Block (PCB) for the PADFL database. It provides fields for DBD name, segment level, PCB... | [Full docs](../documentation/cpy/PADFLPCB.CPY.md) |
| PASFLPCB | This copybook defines the IMS Program Communication Block (PCB) structure named PASFLPCB for accessing the PASFL hierarchical database via DL/I calls. It specifies standard PCB fields including... | [Full docs](../documentation/cpy/PASFLPCB.CPY.md) |
| PAUTBPCB | This COBOL copybook defines the PAUTBPCB level 01 record layout for an IMS Database Program Communication Block (PCB). It specifies fields used in DL/I calls for database navigation and access in... | [Full docs](../documentation/cpy/PAUTBPCB.CPY.md) |
