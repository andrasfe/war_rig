# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-27 14:44:53.386169

## Purpose

This copybook defines the data structures COPAU1AI and COPAU1AO, which appear to be used for screen input and output related to authorization processing. COPAU1AI defines the input fields with associated length, flag, attribute, and input fields. COPAU1AO redefines COPAU1AI to provide a structure for output fields, including control characters and output values.

**Business Context**: UNKNOWN

## Paragraphs/Procedures

### ~~COPAU1AI~~ (Dead Code)
*Record layout 'COPAU1AI' is never used by any program*

### COPAU1AO
This data structure redefines the COPAU1AI structure to provide a format suitable for output, likely to a screen. It includes control characters (C), presentation attributes (P), highlighting attributes (H), validation attributes (V), and output fields (O) corresponding to the input fields defined in COPAU1AI. The fields include TRNNAMEC, TRNNAMEP, TRNNAMEH, TRNNAMEV, TRNNAMEO, TITLE01C, TITLE01P, TITLE01H, TITLE01V, TITLE01O, CURDATEC, CURDATEP, CURDATEH, CURDATEV, CURDATEO, PGMNAMEC, PGMNAMEP, PGMNAMEH, PGMNAMEV, PGMNAMEO, TITLE02C, TITLE02P, TITLE02H, TITLE02V, TITLE02O, CURTIMEC, CURTIMEP, CURTIMEH, CURTIMEV, CURTIMEO, CARDNUMC, CARDNUMP, CARDNUMH, CARDNUMV, CARDNUMO, AUTHDTC, AUTHDTP, AUTHDTH, AUTHDTV, AUTHDTO, AUTHTMC, AUTHTMP, AUTHTMH, AUTHTMV, AUTHTMO, AUTHRSPC, AUTHRSPP, AUTHRSPH, AUTHRSPV, AUTHRSPV, AUTHRSPO, AUTHRSNC, AUTHRSNP, AUTHRSNH, AUTHRSNV, AUTHRSNO, AUTHCDC, AUTHCDP, AUTHCDH, AUTHCDV, AUTHCDO, AUTHAMTC, AUTHAMTP, AUTHAMTH, AUTHAMTV, AUTHAMTO, POSEMDC, POSEMDP, POSEMDO, AUTHSRCC, AUTHSRCP, AUTHSRCH, AUTHSRCV, AUTHSRCO, MCCCDC, MCCCDP, MCCCDH, MCCCDV, MCCCDO, CRDEXPC, CRDEXPP, CRDEXPH, CRDEXPV, CRDEXPO, AUTHTYPC, AUTHTYPP, AUTHTYPH, AUTHTYPV, AUTHTYPO, TRNIDC, TRNIDP, TRNIDH, TRNIDV, TRNIDO, AUTHMTCC, AUTHMTCP, AUTHMTCH, AUTHMTCV, AUTHMTCO, AUTHFRDC, AUTHFRDP, AUTHFRDH, AUTHFRDV, AUTHFRDO, MERNAMEC, MERNAMEP, MERNAMEH, MERNAMEV, MERNAMEO, MERIDC, MERIDP, MERIDH, MERIDV, MERIDO, MERCITYC, MERCITYP, MERCITYH, MERCITYV, MERCITYO, MERSTC, MERSTP, MERSTH, MERSTV, MERSTO, MERZIPC, MERZIPP, MERZIPH, MERZIPV, MERZIPO, ERRMSGC, ERRMSGP, ERRMSGH, ERRMSGV, ERRMSGO. The purpose of this structure is to format the data for display on a screen, including attributes for highlighting, color, and other visual cues. No calls are made from this paragraph, as it is a data structure definition.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU1AI | record_layout | 1 | Record layout 'COPAU1AI' is never used by any program |

## Open Questions

- ? What is the specific screen or application that uses this copybook?
  - Context: The copybook defines data structures for screen input and output, but the specific application is not clear.
- ? What are the valid values and meanings of the flag fields (e.g., TRNNAMEF)?
  - Context: The copybook includes flag fields, but their specific meanings are not documented.
