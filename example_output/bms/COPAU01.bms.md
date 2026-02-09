# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-02-09 15:46:48.711315

## Purpose

This BMS map defines the COPAU01 screen, which displays pending authorization details for a card transaction. The screen includes fields for transaction details, card information, authorization responses, merchant details, and function key options for navigation and fraud management.

## Paragraphs/Procedures

### COPAU1A DFHMDI
This DFHMDI macro defines the map's characteristics, including its size and position on the screen. It specifies that the map occupies the entire screen, which is 24 lines by 80 columns. The map is positioned starting at line 1 and column 1. This macro acts as a container for the individual fields defined by the DFHMDF macros. It sets the overall dimensions and starting point for the screen layout. The attributes defined here affect the entire screen's presentation. No specific business logic or error handling is present within this macro definition. It does not call any other paragraphs or programs; it simply defines the screen's structure.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU01 | map | 19 | Artifact 'COPAU01' (map) is never referenced by any other artifact in the dependency graph |
| COPAU1A | screen | 26 | Screen/Map 'COPAU1A' is never sent to or received from by any program |

## Open Questions

- ? How are the data fields populated on the screen?
  - Context: The BMS map defines the screen layout, but the program logic that populates the fields is not present in this file.
