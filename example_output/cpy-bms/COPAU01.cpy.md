# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-04 04:45:09.035410

## Purpose

This copybook, COPAU01.cpy, defines the data structures COPAU1AI and COPAU1AO, which are used for screen input and output related to authorization processing. The copybook includes fields for transaction name, titles, current date and time, card number, authorization details, merchant information, and error messages.

**Business Context**: This copybook is likely used in online CICS applications related to credit card authorization and transaction processing.

## Paragraphs/Procedures

### ~~COPAU1AI~~ (Dead Code)
*Record layout 'COPAU1AI' is never used by any program*

### COPAU1AO
This data structure redefines COPAU1AI and defines the output fields for a CICS screen related to authorization processing. It uses the same storage as COPAU1AI but provides a different view of the data, allowing the program to format the output for display. The fields are named with suffixes 'C', 'P', 'H', 'V', and 'O' to represent control characters, protected attributes, highlight attributes, validation attributes, and output values, respectively. This structure includes output fields for transaction name (TRNNAMEC, TRNNAMEP, TRNNAMEH, TRNNAMEV, TRNNAMEO), titles (TITLE01C, TITLE01P, TITLE01H, TITLE01V, TITLE01O, TITLE02C, TITLE02P, TITLE02H, TITLE02V, TITLE02O), current date and time (CURDATEC, CURDATEP, CURDATEH, CURDATEV, CURDATEO, CURTIMEC, CURTIMEP, CURTIMEH, CURTIMEV, CURTIMEO), card number (CARDNUMC, CARDNUMP, CARDNUMH, CARDNUMV, CARDNUMO), authorization details (AUTHDTC, AUTHDTP, AUTHDTH, AUTHDTV, AUTHDTO, AUTHTMC, AUTHTMP, AUTHTMH, AUTHTMV, AUTHTMO, AUTHRSPC, AUTHRSPP, AUTHRSPH, AUTHRSPV, AUTHRSPO, AUTHRSNC, AUTHRSNP, AUTHRSNH, AUTHRSNV, AUTHRSNO, AUTHCDC, AUTHCDP, AUTHCDH, AUTHCDV, AUTHCDO, AUTHAMTC, AUTHAMTP, AUTHAMTH, AUTHAMTV, AUTHAMTO), POS entry mode (POSEMDC, POSEMDP, POSEMDH, POSEMDV, POSEMDO), authorization source (AUTHSRCC, AUTHSRCP, AUTHSRCH, AUTHSRCV, AUTHSRCO), MCC code (MCCCDC, MCCCDP, MCCCDH, MCCCDV, MCCCDO), card expiry (CRDEXPC, CRDEXPP, CRDEXPH, CRDEXPV, CRDEXPO), authorization type (AUTHTYPC, AUTHTYPP, AUTHTYPH, AUTHTYPV, AUTHTYPO), transaction ID (TRNIDC, TRNIDP, TRNIDH, TRNIDV, TRNIDO), authorization match (AUTHMTCC, AUTHMTCP, AUTHMTCH, AUTHMTCV, AUTHMTCO), authorization fraud (AUTHFRDC, AUTHFRDP, AUTHFRDH, AUTHFRDV, AUTHFRDO), merchant information (MERNAMEC, MERNAMEP, MERNAMEH, MERNAMEV, MERNAMEO, MERIDC, MERIDP, MERIDH, MERIDV, MERIDO, MERCITYC, MERCITYP, MERCITYH, MERCITYV, MERCITYO, MERSTC, MERSTP, MERSTH, MERSTV, MERSTO, MERZIPC, MERZIPP, MERZIPH, MERZIPV, MERZIPO), and error messages (ERRMSGC, ERRMSGP, ERRMSGH, ERRMSGV, ERRMSGO).

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU1AI | record_layout | 1 | Record layout 'COPAU1AI' is never used by any program |
