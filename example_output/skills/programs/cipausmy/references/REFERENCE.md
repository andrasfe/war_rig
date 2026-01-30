# CIPAUSMY - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** CIPAUSMY
- **File Name:** cpy/CIPAUSMY.cpy
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:46:36.136517

## Purpose

**Summary:** This copybook defines the data structure for the IMS segment 'PENDING AUTHORIZATION SUMMARY'. It includes fields for account ID (PA-ACCT-ID), customer ID (PA-CUST-ID), authorization status (PA-AUTH-STATUS), an array of account statuses (PA-ACCOUNT-STATUS OCCURS 5 TIMES), credit and cash limits and balances, counts of approved/declined authorizations, and corresponding amounts. A filler field pads the structure to the required length.

**Business Context:** Supports management of pending authorizations for customer accounts in an IMS database, tracking statuses, limits, balances, and authorization history for credit and cash transactions.
**Program Type:** UTILITY

## Paragraphs

### CIPAUSMY

[Citadel] Paragraph identified by static analysis

### 05:PA-ACCT-ID

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Dead Code

- **05:PA-ACCT-ID** (column): Artifact '05:PA-ACCT-ID' (column) is never referenced by any other artifact in the dependency graph
