# CBPAUP0C

**File**: `cbl/CBPAUP0C.cbl`
**Type**: FileType.COBOL
**Analyzed**: 2026-01-25 18:35:06.670261

## Purpose

Batch IMS program that sequentially reads pending authorization summary root segments (PAUTSUM0), then child detail segments (PAUTDTL1), checks detail age against expiry parameter, deletes expired details via DLET, adjusts summary counters in memory, deletes summaries with zero remaining counts via DLET, and takes IMS checkpoints periodically.

**Business Context**: CardDemo Authorization Module: deletes expired pending authorization messages to clean up IMS database.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| SYSIN | IOType.PARAMETER | Program parameters: expiry days (P-EXPIRY-DAYS), checkpoint frequency (P-CHKP-FREQ), display frequency (P-CHKP-DIS-FREQ), debug flag (P-DEBUG-FLAG) |
| PAUTSUM0 | IOType.IMS_SEGMENT | Pending authorization summary root segments containing account ID, approved/declined counts and amounts |
| PAUTDTL1 | IOType.IMS_SEGMENT | Pending authorization detail child segments containing auth date, response code, transaction amount |
| SYSTEM_DATE | IOType.OTHER | Current YYDDD date from system for expiry calculation |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PAUTDTL1 | IOType.IMS_SEGMENT | Deletion of expired pending authorization detail segments via DLET |
| PAUTSUM0 | IOType.IMS_SEGMENT | Deletion of empty pending authorization summary segments via DLET |
| CONSOLE | IOType.REPORT | Display of processing statistics: summaries/details read/deleted and checkpoints |
| RETURN_CODE | IOType.RETURN_CODE | Set to 16 on abend conditions |

## Business Rules

- **BR001**: Qualify detail segment for deletion if days elapsed since authorization date exceeds or equals expiry days parameter
- **BR002**: On qualified detail deletion, decrement appropriate summary counters in memory: approved count/amount if response code '00', else declined count/amount
- **BR003**: Delete summary segment after processing all details if both approved auth count <= 0 (note: code uses PA-APPROVED-AUTH-CNT twice, likely typo for declined)
- **BR004**: Take IMS checkpoint after every P-CHKP-FREQ summaries processed, display every P-CHKP-DIS-FREQ checkpoints if debug on

## Paragraphs/Procedures

### MAIN-PARA
Orchestrates initialization, loops over summaries and details to check/delete expired auths, final stats display

### 1000-INITIALIZE
Accepts date parms from SYSIN, sets defaults for expiry/chkpt/debug, displays start info

### 2000-FIND-NEXT-AUTH-SUMMARY
GN call to read next PAUTSUM0 summary segment, increments read counter, sets end flag on GB

### 3000-FIND-NEXT-AUTH-DTL
GNP call to read next PAUTDTL1 detail under current summary, sets more/no-more flags

### 4000-CHECK-IF-EXPIRED
Calculates age of detail auth, sets delete flag, adjusts summary counters in memory if qualified

### 5000-DELETE-AUTH-DTL
DLET current detail segment, increments delete counter

### 6000-DELETE-AUTH-SUMMARY
DLET current summary segment if empty post-adjustments, increments delete counter

### 9000-TAKE-CHECKPOINT
CHKP call with ID, increments counter, conditional display on frequency

### 9999-ABEND
Displays abend message, sets RETURN-CODE 16

## Open Questions

- ? Exact field layouts and data types in copybooks CIPAUSMY and CIPAUDTY
  - Context: Fields like PA-ACCT-ID, PA-AUTH-DATE-9C referenced but definitions not in source
- ? Is line 156 bug? Uses PA-APPROVED-AUTH-CNT twice instead of PA-DECLINED-AUTH-CNT
  - Context: Logic implies check both counters but code repeats approved
