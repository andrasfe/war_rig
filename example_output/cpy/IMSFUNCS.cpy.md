# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 15:15:09.421309

## Purpose

This COBOL copybook defines the FUNC-CODES group (01 level) containing elementary 05-level constant function codes for IMS DL/I database calls, including Get Unique (GU), Get Hold Unique (GHU), Get Next (GN), Get Hold Next (GHN), Get Next Parent (GNP), Get Hold Next Parent (GHNP), Replace (REPL), Insert (ISRT), and Delete (DLET). It also defines PARMCOUNT as a constant numeric value of 4 in S9(05) COMP-5 format, likely specifying the expected parameter count for certain IMS calls. These constants are intended to be copied into COBOL programs for standardized IMS DL/I PCB mask function codes.

**Business Context**: Supports IMS hierarchical database access operations in mainframe COBOL applications, ensuring consistent function code values across programs interacting with IMS DL/I databases.

## Business Rules

- **BR001**: FUNC-GU defines the constant 'GU  ' for IMS DL/I Get Unique operation, retrieving a unique database segment by key.
- **BR002**: FUNC-GHU defines the constant 'GHU ' for IMS DL/I Get Hold Unique operation, retrieving and holding a unique database segment by key.
- **BR003**: FUNC-GN defines the constant 'GN  ' for IMS DL/I Get Next operation, retrieving the next sequential database segment.
- **BR004**: FUNC-GHN defines the constant 'GHN ' for IMS DL/I Get Hold Next operation, retrieving and holding the next sequential database segment.
- **BR005**: FUNC-GNP defines the constant 'GNP ' for IMS DL/I Get Next within Parent operation, retrieving the next segment at the same level under the parent.
- **BR006**: FUNC-GHNP defines the constant 'GHNP' for IMS DL/I Get Hold Next within Parent operation, retrieving and holding the next segment at the same level under the parent.
- **BR007**: FUNC-REPL defines the constant 'REPL' for IMS DL/I Replace operation, updating an existing database segment.
- **BR008**: FUNC-ISRT defines the constant 'ISRT' for IMS DL/I Insert operation, adding a new database segment.
- **BR009**: FUNC-DLET defines the constant 'DLET' for IMS DL/I Delete operation, removing a database segment.
- **BR010**: PARMCOUNT defines a constant value of 4, likely governing the number of parameters passed in specific IMS DL/I calls using these function codes.
