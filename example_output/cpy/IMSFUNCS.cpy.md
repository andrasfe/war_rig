# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 17:39:06.546163

## Purpose

This COBOL copybook defines a data structure FUNC-CODES containing standardized 4-character IMS DL/I function codes for database operations including Get Unique (GU), Get Hold Unique (GHU), Get Next (GN), Get Hold Next (GHN), Get Next within Parent (GNP), Get Hold Next within Parent (GHNP), Replace (REPL), Insert (ISRT), and Delete (DLET). It also defines a PARMCOUNT field with an initial value of 4 in COMP-5 format, likely specifying the number of parameters for IMS calls. These constants are used in programs performing IMS database access to ensure consistent function code values.

**Business Context**: IMS hierarchical database (DL/I) interface standardization for COBOL programs handling get, insert, delete, and update operations on IMS segments.

## Open Questions

- ? In which specific programs or modules is this copybook included?
  - Context: The copybook defines reusable constants but does not indicate usage locations within this file.
