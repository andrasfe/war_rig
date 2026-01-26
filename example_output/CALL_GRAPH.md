# Call Graph Analysis

*Generated: 2026-01-25 19:36:30*

**Programs Analyzed:** 13

## Visual Call Graph

```mermaid
flowchart TD

    subgraph jobs[" "]
        COPAUA0C([COPAUA0C])
        COPAUS0C([COPAUS0C])
        COPAUS1C([COPAUS1C])
        DBPAUTP0([DBPAUTP0])
        DBUNLDGS([DBUNLDGS])
        PAUDBLOD([PAUDBLOD])
        PAUDBUNL([PAUDBUNL])
        UNLDPADB([UNLDPADB])
    end

    subgraph external[" "]
        CBLTDLI>CBLTDLI]
        CDEMO_TO_PROGRAM>CDEMO-TO-PROGRAM]
        DFSRRC00>DFSRRC00]
        MQCLOSE>MQCLOSE]
        MQGET>MQGET]
        MQOPEN>MQOPEN]
        MQPUT1>MQPUT1]
        WS_PGM_AUTH_FRAUD>WS-PGM-AUTH-FRAUD]
    end

    %% Call relationships
    COPAUA0C --> MQOPEN
    COPAUA0C --> MQGET
    COPAUA0C --> MQPUT1
    COPAUA0C --> MQCLOSE
    COPAUS0C --> CDEMO_TO_PROGRAM
    COPAUS0C --> CDEMO_TO_PROGRAM
    COPAUS1C --> WS_PGM_AUTH_FRAUD
    COPAUS1C --> CDEMO_TO_PROGRAM
    DBPAUTP0 --> DFSRRC00
    DBUNLDGS --> CBLTDLI
    DBUNLDGS --> CBLTDLI
    DBUNLDGS --> CBLTDLI
    DBUNLDGS --> CBLTDLI
    PAUDBLOD --> CBLTDLI
    PAUDBLOD --> CBLTDLI
    PAUDBLOD --> CBLTDLI
    PAUDBUNL --> CBLTDLI
    PAUDBUNL --> CBLTDLI
    UNLDPADB --> DFSRRC00
    UNLDPADB --> DFSRRC00
    UNLDPADB --> DFSRRC00
    UNLDPADB --> DFSRRC00

    %% Styling
    classDef entryPoint fill:#90EE90,stroke:#228B22
    class CBPAUP0C,CBPAUP0J,COPAUA0C,COPAUS0C,COPAUS1C,COPAUS2C,DBPAUTP0,DBUNLDGS,LOADPADB,PAUDBLOD,PAUDBUNL,UNLDGSAM,UNLDPADB entryPoint
    classDef missing fill:#1E3A5F,stroke:#2E5A8F,color:#FFFFFF
    class CBLTDLI,CDEMO_TO_PROGRAM,DFSRRC00,MQCLOSE,MQGET,MQOPEN,MQPUT1,WS_PGM_AUTH_FRAUD missing
```

## Entry Points

- ✓ **CBPAUP0C**: This batch IMS COBOL program deletes expired pending authorization detail segmen...
- ✓ **CBPAUP0J**: This JCL defines a batch job that executes the IMS region controller DFSRRC00 to...
- ✓ **COPAUA0C**: This COBOL source file is empty, containing only a single blank line at line 1. ...
- ✓ **COPAUS0C**: This CICS/IMS/BMS COBOL program provides a paginated summary view of pending aut...
- ✓ **COPAUS1C**: COPAUS1C is a CICS/IMS/BMS online program that provides a detail view of pending...
- ✓ **COPAUS2C**: This CICS COBOL program processes authorization messages to mark them as fraud b...
- ✓ **DBPAUTP0**: JCL batch job that first deletes any existing unload dataset AWS.M2.CARDDEMO.IMS...
- ✓ **DBUNLDGS**: This IMS batch program sequentially reads all root segments (PAUTSUM0) from the ...
- ✓ **LOADPADB**: This JCL submits a batch job to execute the IMS utility program DFSRRC00 in BMP ...
- ✓ **PAUDBLOD**: This batch COBOL program loads pending authorization data into an IMS hierarchic...
- ✓ **PAUDBUNL**: This utility program unloads root segments (PAUTSUM0 Pending Authorization Summa...
- ✓ **UNLDGSAM**: This JCL submits a batch job to execute the IMS utility program DFSRRC00 in DLI ...
- ✓ **UNLDPADB**: This JCL job executes an IMS database unload using the DFSRRC00 utility to extra...

## External Dependencies

### System Utilities (Skipped)

*None*

### Custom Programs (Need Documentation)

- **CBLTDLI**: Called by PAUDBLOD (EXEC), PAUDBLOD (EXEC), PAUDBLOD (EXEC), DBUNLDGS (EXEC), DBUNLDGS (EXEC), DBUNLDGS (EXEC), DBUNLDGS (EXEC), PAUDBUNL (EXEC), PAUDBUNL (EXEC)
- **CDEMO-TO-PROGRAM**: Called by COPAUS1C (EXEC), COPAUS0C (EXEC), COPAUS0C (EXEC)
- **DFSRRC00**: Called by DBPAUTP0 (EXEC), UNLDPADB (EXEC), UNLDPADB (EXEC), UNLDPADB (EXEC), UNLDPADB (EXEC)
- **MQCLOSE**: Called by COPAUA0C (EXEC)
- **MQGET**: Called by COPAUA0C (EXEC)
- **MQOPEN**: Called by COPAUA0C (EXEC)
- **MQPUT1**: Called by COPAUA0C (EXEC)
- **WS-PGM-AUTH-FRAUD**: Called by COPAUS1C (EXEC)

## Statistics

| Metric | Count |
|--------|-------|
| Documented Programs | 13 |
| Entry Points | 13 |
| Leaf Nodes | 13 |
| External Dependencies | 8 |
| System Utilities | 0 |
| Auto-classified | 0 |
| Custom Missing | 8 |
| Total Calls | 22 |

### Status Legend

- ✓ **Documented**: Fully documented from source file
- ~ **Internal**: Found as routine/section in parent program
- ✗ **External**: External or missing program
- ⚙ **System**: Known system utility
