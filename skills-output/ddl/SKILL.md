---
name: ddl
description: DDL documentation (database definitions)
---

# DDL Documentation

| Name | Description | Documentation |
|---------|-------------|---------------|
| AUTHFRDS | DDL script that creates the CARDDEMO.AUTHFRDS DB2 table for storing authorization transaction details related to fraud detection. The table captures card number, timestamp, merchant data,... | [Full docs](../documentation/ddl/AUTHFRDS.ddl.md) |
| XAUTHFRD | This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index is defined on columns CARD_NUM in ascending order and AUTH_TS in descending order. The COPY... | [Full docs](../documentation/ddl/XAUTHFRD.ddl.md) |
