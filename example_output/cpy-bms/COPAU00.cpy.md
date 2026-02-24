# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-24 17:38:57.497823

## Purpose

This copybook defines the BMS map structure for screen input and output related to an UNKNOWN program. It likely contains field definitions, attributes, and screen layout information used by CICS applications.

## Paragraphs/Procedures

### ~~COPAU0AI~~ (Dead Code)
*Record layout 'COPAU0AI' is never used by any program*

### COPAU0AO
This data structure, which redefines COPAU0AI, defines the output layout for the BMS map. It contains similar fields as COPAU0AI, but the fields are structured for output to the CICS terminal. The fields ending in 'C', 'P', 'H', and 'V' are attribute characters used to control the display characteristics of the output fields. The fields ending in 'O' represent the actual output data to be displayed on the screen. This structure is used to format and send data to the CICS terminal for display.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU0AI | record_layout | 1 | Record layout 'COPAU0AI' is never used by any program |

## Open Questions

- ? What is the purpose of this BMS map?
  - Context: The code provides no context about the application or function of this map.
- ? What are the specific definitions and attributes of the fields within the map?
  - Context: The provided code snippet is incomplete, lacking the actual field definitions.
