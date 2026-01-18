# CSUTLDTC

**File:** CSUTLDTC.cbl
**Type:** COBOL
**Status:** FinalStatus.VALHALLA
**Iterations:** 2
**Analyzed:** 2026-01-18 17:41:40.198279

## Purpose

This program is a COBOL subroutine that wraps the CEEDAYS API to validate a Gregorian date string (provided via linkage) against a specified format mask. It prepares VSTRING inputs, invokes CEEDAYS to compute Lillian date (discarding the result), processes the feedback code to populate a diagnostic message in LS-RESULT, and sets RETURN-CODE to severity.

**Business Context:** Facilitates standardized date string validation in mainframe legacy systems, converting to Lillian date format for reliable date arithmetic, validation, and error reporting in business processes like financial transactions or reporting where date accuracy is critical.
**Program Type:** SUBROUTINE
**Citations:** Lines 20, 83, 84, 85, 86, 88, 93, 94, 97, 98, 100, 103, 116

## Calling Context

**Linkage Section:** LS-DATE, LS-DATE-FORMAT, LS-RESULT

## Inputs

### LS-DATE
- **Type:** PARAMETER
- **Description:** 10-character input date string to be validated by CEEDAYS
- **Lines:** 84, 105, 106, 107

### LS-DATE-FORMAT
- **Type:** PARAMETER
- **Description:** 10-character date format mask for interpreting LS-DATE
- **Lines:** 85, 109, 110, 111

## Outputs

### LS-RESULT
- **Type:** PARAMETER
- **Description:** 80-character output message containing severity, message number, result description, test date, and format mask
- **Lines:** 86, 97

### RETURN-CODE
- **Type:** RETURN_CODE
- **Description:** Severity value extracted from CEEDAYS feedback code
- **Lines:** 98

## Called Programs

| Program | Call Type | Purpose | Line |
|---------|-----------|---------|------|
| [CEEDAYS](./CEEDAYS.md) | STATIC_CALL | Validates date string to Lillian date conversion based on input date and format mask, providing feedback code | 116 |

## Business Rules

### BR001: If CEEDAYS feedback is zero (FC-INVALID-DATE), classify date as valid
**Logic:** EVALUATE sets WS-RESULT to 'Date is valid'
**Conditions:** FC-INVALID-DATE
**Lines:** 62, 129, 130

### BR002: If CEEDAYS feedback indicates insufficient data, report 'Insufficient'
**Logic:** EVALUATE sets WS-RESULT to 'Insufficient'
**Conditions:** FC-INSUFFICIENT-DATA
**Lines:** 63, 131, 132

### BR003: If CEEDAYS feedback indicates bad date value, report 'Datevalue error'
**Logic:** EVALUATE sets WS-RESULT to 'Datevalue error'
**Conditions:** FC-BAD-DATE-VALUE
**Lines:** 64, 133, 134

### BR004: If CEEDAYS feedback indicates invalid era, report 'Invalid Era    '
**Logic:** EVALUATE sets WS-RESULT to 'Invalid Era    '
**Conditions:** FC-INVALID-ERA
**Lines:** 65, 135, 136

### BR005: If CEEDAYS feedback indicates unsupported range, report 'Unsupp. Range  '
**Logic:** EVALUATE sets WS-RESULT to 'Unsupp. Range  '
**Conditions:** FC-UNSUPP-RANGE
**Lines:** 66, 137, 138

### BR006: If CEEDAYS feedback indicates invalid month, report 'Invalid month  '
**Logic:** EVALUATE sets WS-RESULT to 'Invalid month  '
**Conditions:** FC-INVALID-MONTH
**Lines:** 67, 139, 140

### BR007: If CEEDAYS feedback indicates bad picture string, report 'Bad Pic String '
**Logic:** EVALUATE sets WS-RESULT to 'Bad Pic String '
**Conditions:** FC-BAD-PIC-STRING
**Lines:** 68, 141, 142

### BR008: If CEEDAYS feedback indicates non-numeric data, report 'Nonnumeric data'
**Logic:** EVALUATE sets WS-RESULT to 'Nonnumeric data'
**Conditions:** FC-NON-NUMERIC-DATA
**Lines:** 69, 143, 144

### BR009: If CEEDAYS feedback indicates year in era zero, report 'YearInEra is 0 '
**Logic:** EVALUATE sets WS-RESULT to 'YearInEra is 0 '
**Conditions:** FC-YEAR-IN-ERA-ZERO
**Lines:** 70, 145, 146

### BR010: For any other unrecognized CEEDAYS feedback code, default to 'Date is invalid'
**Logic:** EVALUATE WHEN OTHER sets WS-RESULT to 'Date is invalid'
**Conditions:** OTHER
**Lines:** 147, 148

## Data Flow

### Reads From
- **LS-DATE**: LS-DATE
  (Lines: 105, 107)
- **LS-DATE-FORMAT**: LS-DATE-FORMAT
  (Lines: 109, 111)

### Writes To
- **LS-RESULT**: WS-MESSAGE
  (Lines: 97)
- **RETURN-CODE**: WS-SEVERITY-N
  (Lines: 98)

### Transformations
- **LS-DATE** → **WS-DATE-TO-TEST**: Length moved to Vstring-length and content to Vstring-text for CEEDAYS input
  (Lines: 105, 106, 107)
- **LS-DATE-FORMAT** → **WS-DATE-FORMAT**: Length moved to Vstring-length and content to Vstring-text for CEEDAYS input
  (Lines: 109, 110, 111)
- **SEVERITY OF FEEDBACK-CODE** → **WS-SEVERITY-N**: Extracts severity code from FEEDBACK-CODE structure to WS-MESSAGE severity field
  (Lines: 123)
- **MSG-NO OF FEEDBACK-CODE** → **WS-MSG-NO-N**: Extracts message number from FEEDBACK-CODE structure to WS-MESSAGE message number field
  (Lines: 124)
- **FEEDBACK-CODE** → **WS-MESSAGE**: EVALUATE maps condition to WS-RESULT text based on 88-levels; WS-DATE-TO-TEST copied to WS-DATE; WS-DATE-FORMAT to WS-DATE-FMT
  (Lines: 122, 128)

## Key Paragraphs

### A000-MAIN
**Purpose:** Prepares WS-DATE-TO-TEST and WS-DATE-FORMAT VSTRINGs from linkage inputs, calls CEEDAYS, extracts feedback details, and evaluates condition to set result message
- Called by: PROCEDURE DIVISION at line 93
- Lines: 103-151

### A000-MAIN-EXIT
**Purpose:** Provides THRU exit point after A000-MAIN processing
- Lines: 152-154

## Error Handling

- **FC-INVALID-DATE (zero feedback):** Sets WS-RESULT to 'Date is valid'; populates WS-MESSAGE with details; sets RETURN-CODE to severity; continues normally
  (Lines: 62, 129, 130)
- **FC-INSUFFICIENT-DATA:** Sets WS-RESULT to 'Insufficient'; populates WS-MESSAGE; sets RETURN-CODE to severity; continues normally
  (Lines: 63, 131, 132)
- **FC-BAD-DATE-VALUE:** Sets WS-RESULT to 'Datevalue error'; populates WS-MESSAGE; sets RETURN-CODE to severity; continues normally
  (Lines: 64, 133, 134)
- **FC-INVALID-ERA:** Sets WS-RESULT to 'Invalid Era    '; populates WS-MESSAGE; sets RETURN-CODE to severity; continues normally
  (Lines: 65, 135, 136)
- **FC-UNSUPP-RANGE:** Sets WS-RESULT to 'Unsupp. Range  '; populates WS-MESSAGE; sets RETURN-CODE to severity; continues normally
  (Lines: 66, 137, 138)
- **FC-INVALID-MONTH:** Sets WS-RESULT to 'Invalid month  '; populates WS-MESSAGE; sets RETURN-CODE to severity; continues normally
  (Lines: 67, 139, 140)
- **FC-BAD-PIC-STRING:** Sets WS-RESULT to 'Bad Pic String '; populates WS-MESSAGE; sets RETURN-CODE to severity; continues normally
  (Lines: 68, 141, 142)
- **FC-NON-NUMERIC-DATA:** Sets WS-RESULT to 'Nonnumeric data'; populates WS-MESSAGE; sets RETURN-CODE to severity; continues normally
  (Lines: 69, 143, 144)
- **FC-YEAR-IN-ERA-ZERO:** Sets WS-RESULT to 'YearInEra is 0 '; populates WS-MESSAGE; sets RETURN-CODE to severity; continues normally
  (Lines: 70, 145, 146)
- **Any other FEEDBACK-CODE:** Sets WS-RESULT to 'Date is invalid'; populates WS-MESSAGE; sets RETURN-CODE to severity; continues normally
  (Lines: 147, 148)

## Open Questions

- **Precise meaning of Lillian date format and CEEDAYS format masks**
  - Context: Not defined in source code; only referenced as output type and input format
  - Suggestion: Consult LE/CEEDAYS documentation or run with sample inputs
- **Why FC-INVALID-DATE (value zeros) maps to 'Date is valid'**
  - Context: Condition name suggests error but zero value and success message indicate it is the success case
  - Suggestion: Verify against CEEDAYS API specification

---
*Generated by War Rig WAR_RIG*