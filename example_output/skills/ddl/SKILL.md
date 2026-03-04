---
name: ddl
description: DDL documentation (database definitions)
---

# DDL Documentation

| Name | Description | Documentation |
|---------|-------------|---------------|
| AUTHFRDS | This DDL file defines the CARDDEMO.AUTHFRDS table, which stores authorization and fraud-related data for card transactions. The table includes fields for card number, authorization timestamp,... | [Full docs](../documentation/ddl/AUTHFRDS.ddl.md) |
| XAUTHFRD | This DDL file defines a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index is composed of the CARD_NUM column in ascending order and the AUTH_TS column in descending... | [Full docs](../documentation/ddl/XAUTHFRD.ddl.md) |
