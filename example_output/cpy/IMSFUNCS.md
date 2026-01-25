# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-25 18:35:43.405426

## Purpose

This copybook defines the FUNC-CODES group containing fixed 4-character IMS DL/I function codes such as 'GU  ' for Get Unique and 'GHU ' for Get Hold Unique. It also defines PARMCOUNT as 4, likely indicating the number of parameters expected in related IMS calls.

**Business Context**: IMS DL/I database access operations, providing standardized function code constants

## Business Rules

- **BR001**: FUNC-GU is predefined to 'GU  ' for IMS Get Unique function
- **BR002**: FUNC-GHU is predefined to 'GHU ' for IMS Get Hold Unique function
- **BR003**: FUNC-GN is predefined to 'GN  ' for IMS Get Next function
- **BR004**: FUNC-GHN is predefined to 'GHN ' for IMS Get Hold Next function
- **BR005**: FUNC-GNP is predefined to 'GNP ' for IMS Get Next within Parent function
- **BR006**: FUNC-GHNP is predefined to 'GHNP' for IMS Get Hold Next within Parent function
- **BR007**: FUNC-REPL is predefined to 'REPL' for IMS Replace function
- **BR008**: FUNC-ISRT is predefined to 'ISRT' for IMS Insert function
- **BR009**: FUNC-DLET is predefined to 'DLET' for IMS Delete function
- **BR010**: PARMCOUNT is predefined to +4, indicating expected parameter count for IMS calls using these functions

## Open Questions

- ? Which programs or modules include this copybook?
  - Context: Usage context cannot be determined from the copybook source alone
