# CCPAURLY - Complete Reference

*This file contains detailed technical documentation. See SKILL.md for a summary.*

## Program Header

- **Program ID:** CCPAURLY
- **File Name:** cpy/CCPAURLY.cpy
- **File Type:** COPYBOOK
- **Analyzed By:** WAR_RIG
- **Analyzed At:** 2026-01-30T19:43:13.422702

## Purpose

**Summary:** This COBOL copybook defines data structures at level 05 for a Pending Authorization Response record. It includes elementary fields for card number (PIC X(16)), transaction ID (PIC X(15)), authorization ID code (PIC X(06)), authorization response code (PIC X(02)), response reason (PIC X(04)), and approved amount (PIC +9(10).99). The fields share a 'PA-RL-' prefix, logically grouping them for use in payment authorization processing.

**Business Context:** Payment card authorization processing, capturing response details from authorization requests
**Program Type:** UTILITY

## Paragraphs

### CCPAURLY

[Citadel] Paragraph identified by static analysis

### 05:PA-RL-CARD-NUM

**(Dead Code)**

[Citadel] Paragraph identified by static analysis

## Data Flow

## Dead Code

- **05:PA-RL-CARD-NUM** (column): Artifact '05:PA-RL-CARD-NUM' (column) is never referenced by any other artifact in the dependency graph
