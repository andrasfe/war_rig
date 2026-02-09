---
name: copybook
description: Copybook documentation (shared data structures)
---

# COPYBOOK Documentation

| Name | Description | Documentation |
|---------|-------------|---------------|
| CCPAUERY | This copybook defines the structure of an error log record, used for logging errors and warnings within an application. It includes fields for date, time, application name, program name, location,... | [Full docs](../documentation/cpy/CCPAUERY.cpy.md) |
| CCPAURLY | This copybook defines the data structure for a pending authorization response, including fields for card number, transaction ID, authorization ID code, response code and reason, and approved... | [Full docs](../documentation/cpy/CCPAURLY.cpy.md) |
| CCPAURQY | This copybook defines the data structure for a pending authorization request. It includes fields for transaction details such as card number, amount, merchant information, and date/time of... | [Full docs](../documentation/cpy/CCPAURQY.cpy.md) |
| CIPAUDTY | This copybook defines the data structure for the IMS segment related to pending authorization details. It includes fields for authorization keys, card details, transaction information, merchant... | [Full docs](../documentation/cpy/CIPAUDTY.cpy.md) |
| CIPAUSMY | This copybook defines the structure of the IMS segment for pending authorization summary data. It includes fields for account ID, customer ID, authorization status, account statuses (occurs 5... | [Full docs](../documentation/cpy/CIPAUSMY.cpy.md) |
| IMSFUNCS | This copybook defines a set of constants representing IMS function codes and a parameter count. These constants are used when making calls to IMS databases to specify the desired operation. | [Full docs](../documentation/cpy/IMSFUNCS.cpy.md) |
| PADFLPCB | This copybook defines the PADFLPCB data structure, which appears to be related to IMS PCB (Program Communication Block) information. It contains fields for DBD name, segment level, PCB status,... | [Full docs](../documentation/cpy/PADFLPCB.CPY.md) |
| PASFLPCB | This copybook defines the data structure PASFLPCB, which appears to be related to IMS PCB (Program Communication Block) information. It contains fields for DBD name, segment level, PCB status,... | [Full docs](../documentation/cpy/PASFLPCB.CPY.md) |
| PAUTBPCB | This copybook defines the structure of the PAUTBPCB data area, which appears to be related to IMS database processing, containing fields for database name, segment level, PCB status, processing... | [Full docs](../documentation/cpy/PAUTBPCB.CPY.md) |
