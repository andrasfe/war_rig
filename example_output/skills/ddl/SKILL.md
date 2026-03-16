---
name: ddl
description: DDL documentation (database definitions)
---

# DDL Documentation

| Name | Description | Documentation |
|---------|-------------|---------------|
| AUTHFRDS | This DDL file defines the CARDDEMO.AUTHFRDS table, which stores authorization and fraud-related data for card transactions. The table includes fields for card number, authorization timestamp,... | [Full docs](../documentation/ddl/AUTHFRDS.ddl.md) |
| XAUTHFRD | This DDL file creates a unique index named XAUTHFRD on the AUTHFRDS table in the CARDDEMO schema. The index is defined on the CARD_NUM and AUTH_TS columns, sorted in ascending and descending order... | [Full docs](../documentation/ddl/XAUTHFRD.ddl.md) |
