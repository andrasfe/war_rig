---
name: cbpaup0c
description: "Batch IMS program that traverses the authorization database, reads pending authorization summary segments (PAUTSUM0), and for each, reads child detail segments (PAUTDTL1). It checks each detail for expiry based on days since authorization date exceeding a parameter-driven threshold, deletes expired details via DLI DLET, adjusts in-memory summary counters accordingly, and deletes the parent summary if both approved and declined auth counts reach zero after processing all details. Periodically takes IMS checkpoints and displays processing statistics at end."
---

# CBPAUP0C

**Type:** COBOL (BATCH)
**Context:** CardDemo Authorization Module: Cleanup of expired pending authorization messages to maintain database hygiene by removing stale transaction details and empty summaries.

## Purpose

Batch IMS program that traverses the authorization database, reads pending authorization summary segments (PAUTSUM0), and for each, reads child detail segments (PAUTDTL1). It checks each detail for expiry based on days since authorization date exceeding a parameter-driven threshold, deletes expired details via DLI DLET, adjusts in-memory summary counters accordingly, and deletes the parent summary if both approved and declined auth counts reach zero after processing all details. Periodically takes IMS checkpoints and displays processing statistics at end.

## Business Rules

- **BR001**: Delete detail segment if days elapsed since authorization date (CURRENT-YYDDD - (99999 - PA-AUTH-DATE-9C)) >= P-EXPIRY-DAYS (default 5)
- **BR002**: Delete summary segment after all details processed if PA-APPROVED-AUTH-CNT <= 0 (note: code has duplicate condition, likely intended PA-DECLINED-AUTH-CNT <=0 also)
- **BR003**: Take IMS checkpoint every P-CHKP-FREQ (default 5) summaries processed; display status every P-CHKP-DIS-FREQ (default 10) checkpoints

## Inputs

- **PRM-INFO** (PARAMETER): Command line parameters from SYSIN containing expiry days (P-EXPIRY-DAYS), checkpoint frequency (P-CHKP-FREQ), display frequency (P-CHKP-DIS-FREQ), and debug flag (P-DEBUG-FLAG)
- **PENDING-AUTH-SUMMARY** (IMS_SEGMENT): Root segment PAUTSUM0 containing summary data like PA-ACCT-ID, PA-APPROVED-AUTH-CNT, PA-DECLINED-AUTH-CNT, etc., read via GN calls
- **PENDING-AUTH-DETAILS** (IMS_SEGMENT): Child segment PAUTDTL1 under current summary, containing details like PA-AUTH-DATE-9C, PA-AUTH-RESP-CODE, PA-TRANSACTION-AMT, read via GNP calls

## Outputs

- **PENDING-AUTH-DETAILS** (IMS_SEGMENT): Deletion of expired PAUTDTL1 segments via DLI DLET (no inserts/updates)
- **PENDING-AUTH-SUMMARY** (IMS_SEGMENT): Deletion of PAUTSUM0 segments with zero counts after detail processing via DLI DLET (no inserts/updates to counters; adjustments are in-memory only for delete decision)
- **CHECKPOINT** (OTHER): IMS checkpoints taken periodically via DLI CHKP to ensure transaction integrity
- **PROCESSING STATISTICS** (REPORT): Console DISPLAY of counts: summaries read/deleted, details read/deleted

## Copybooks Used

- **CIPAUSMY**: Defines layout of PENDING-AUTH-SUMMARY (PAUTSUM0 root segment) including PA-ACCT-ID, counters for approved/declined auths
- **CIPAUDTY**: Defines layout of PENDING-AUTH-DETAILS (PAUTDTL1 child segment) including PA-AUTH-DATE-9C, PA-AUTH-RESP-CODE, amounts

## When to Use This Skill

Use this skill when you need to:
- Understand the purpose and functionality of CBPAUP0C
- Understand business rules implemented in CBPAUP0C
- Identify inputs/outputs for CBPAUP0C
- Maintain or modify CBPAUP0C

## Additional Details

See [REFERENCE.md](references/REFERENCE.md) for complete technical details including paragraphs, data flow, error handling, and SQL/CICS operations.