# COPAU01 - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** COPAU01
- **File Name:** COPAU01.cpy
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:55:15.224368

## Purpose

**Summary:** This copybook defines the input and output data structures for a CICS BMS map named COPAU1. It contains field definitions for transaction details, dates, times, card numbers, authorization information, merchant details, and error messages, used for both input (COPAU1AI) and output (COPAU1AO) layouts.

**Business Context:** UNKNOWN
**Program Type:** BMS

## Inputs

### COPAU1AI

- **Type:** CICS_MAP
- **Description:** Input data structure for the COPAU1 CICS BMS map, containing fields for transaction name, titles, current date and time, card number, authorization details, merchant information, and error messages.

## Outputs

### COPAU1AO

- **Type:** CICS_MAP
- **Description:** Output data structure for the COPAU1 CICS BMS map, containing fields for transaction name, titles, current date and time, card number, authorization details, merchant information, and error messages, formatted for display.

## Paragraphs

### COPAU01

[Citadel] Paragraph identified by static analysis

### COPAU1AI

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Dead Code

- **COPAU1AI** (record_layout): Record layout 'COPAU1AI' is never used by any program
