# System Design

*Generated: 2026-01-23 12:25:45*

## Overview

This document describes the architecture of the analyzed codebase,
including 13 documented programs 
and their dependencies.

## Program Inventory

| Status | Program | Type | Summary |
|--------|---------|------|---------|
| ✓ | BATCMP | JCL | This JCL defines a job to compile a batch COBOL program usin... |
| ✓ | BLDCIDB2 | PROC | This JCL PROC compiles CICS COBOL programs with embedded DB2... |
| ✓ | BLDONL | PROC | This JCL procedure compiles CICS-enabled COBOL programs usin... |
| ✓ | BUILDBAT | PROC | This JCL procedure compiles a specified batch COBOL program ... |
| ✓ | BUILDBMS | PROC | This JCL procedure compiles a specified BMS map by printing ... |
| ✓ | CBLDBMS | JCL | This JCL job compiles the CICS BMS map named CICSMAP using t... |
| ✓ | CICCMP | JCL | This JCL job compiles a CICS COBOL program (default name CIC... |
| ✓ | CICDBCMP | JCL | This JCL job sets symbolic parameters for libraries, DB2 sub... |
| ✓ | IMSMQCMP | JCL | This JCL job translates, compiles, prepares link cards, and ... |
| ✓ | LISTCAT | JCL | This JCL job generates a catalog listing for all entries und... |
| ✓ | RACFCMDS | JCL | This JCL job RACFCMDS executes the TSO program IKJEFT01 in b... |
| ✓ | REPRTEST | JCL | This JCL job invokes the REPROC procedure to perform a file ... |
| ✓ | SORTTEST | JCL | This JCL defines a batch job that executes the SORT utility ... |

## External Dependencies

### System Utilities

The following system utilities are called but not documented:

- `DFHECP1`
- `DSNHPC`
- `HEWL`
- `IDCAMS`
- `IEBGENER`
- `IEFBR14`
- `IEWL`
- `IGYCRCTL`
- `IKJEFT01`
- `SDSF`

### Missing Custom Programs

The following custom programs are referenced but not documented:

| Program | Called By | Call Type |
|---------|-----------|-----------|
| BUILDONL | CICCMP | OTHER |

> **Note**: These programs should be located and documented to complete the system design.

## Call Graph Summary

- **Entry Points**: 11
- **Leaf Nodes**: 11
- **Total Calls**: 19
- **Documentation Complete**: No
