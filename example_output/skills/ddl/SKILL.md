---
name: ddl
description: DDL documentation (database definitions)
---

# DDL Documentation

| Name | Description | Documentation |
|---------|-------------|---------------|
| AUTHFRDS | This DDL file defines the CARDDEMO.AUTHFRDS table, which stores authorization and fraud-related information for card transactions. It includes fields for card number, authorization timestamp,... | [Full docs](../documentation/ddl/AUTHFRDS.ddl.md) |
| XAUTHFRD | This DDL file creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table, using the CARD_NUM and AUTH_TS columns. The index is created with a COPY YES attribute. | [Full docs](../documentation/ddl/XAUTHFRD.ddl.md) |
