---
name: ddl
description: DDL documentation (database definitions)
---

# DDL Documentation

| Name | Description | Documentation |
|---------|-------------|---------------|
| AUTHFRDS | DDL script that creates the AUTHFRDS table in the CARDDEMO schema to store authorization transaction details for fraud detection and analysis in card processing. Columns capture card details,... | [Full docs](../documentation/ddl/AUTHFRDS.ddl.md) |
| XAUTHFRD | This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index is defined on CARD_NUM in ascending order and AUTH_TS in descending order. The COPY YES... | [Full docs](../documentation/ddl/XAUTHFRD.ddl.md) |
