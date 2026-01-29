# CIPAUSMY - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** CIPAUSMY
- **File Name:** CIPAUSMY.cpy
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-28T14:54:25.868900

## Purpose

**Summary:** This copybook defines the data structure for the IMS segment 'PENDING AUTHORIZATION SUMMARY'. It contains fields related to account identification, authorization status, credit and cash limits/balances, and authorization counts/amounts. The copybook is used for representing pending authorization information within an IMS database.

**Business Context:** None
**Program Type:** SUBROUTINE

## Paragraphs

### DATA-DEFINITION

This section defines the structure of the PENDING AUTHORIZATION SUMMARY IMS segment. It includes fields for account identification (PA-ACCT-ID, PA-CUST-ID), authorization status (PA-AUTH-STATUS), account statuses (PA-ACCOUNT-STATUS), credit and cash limits (PA-CREDIT-LIMIT, PA-CASH-LIMIT), credit and cash balances (PA-CREDIT-BALANCE, PA-CASH-BALANCE), approved and declined authorization counts (PA-APPROVED-AUTH-CNT, PA-DECLINED-AUTH-CNT), approved and declined authorization amounts (PA-APPROVED-AUTH-AMT, PA-DECLINED-AUTH-AMT), and a filler field. The PA-ACCOUNT-STATUS field is defined as an array of 5 two-character fields, allowing for multiple account statuses to be stored. The segment is intended to hold information about pending authorizations for an account, including limits, balances, and authorization history. No specific business logic or error handling is present within this copybook, as it only defines the data structure. There are no calls to other paragraphs or programs within this copybook.

## Data Flow

## Dead Code

- **05:PA-ACCT-ID** (column): Artifact '05:PA-ACCT-ID' (column) is never referenced by any other artifact in the dependency graph
