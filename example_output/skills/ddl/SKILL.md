---
name: ddl
description: DDL documentation (database definitions)
---

# DDL Documentation

| Name | Description | Documentation |
|---------|-------------|---------------|
| AUTHFRDS | This DDL file defines the CARDDEMO.AUTHFRDS table, which stores authorization and fraud-related data for card transactions. The table includes columns for card number, authorization timestamp,... | [Full docs](../documentation/ddl/AUTHFRDS.ddl.md) |
| XAUTHFRD | This DDL file creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table, using the CARD_NUM in ascending order and AUTH_TS in descending order. The index allows copies. | [Full docs](../documentation/ddl/XAUTHFRD.ddl.md) |
