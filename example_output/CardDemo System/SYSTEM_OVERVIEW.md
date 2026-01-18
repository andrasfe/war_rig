# CardDemo System Overview

## Executive Summary

CardDemo is a comprehensive credit card management system designed for financial institutions to handle customer accounts, transactions, and administrative functions. The system provides both online (CICS) and batch processing capabilities to manage the complete lifecycle of credit card operations, from customer onboarding and transaction processing to billing, reporting, and system administration.

The system maintains a normalized data model with core entities including customers, accounts, credit cards, transactions, and cross-reference relationships. It supports typical banking operations such as transaction posting, interest calculation, statement generation, and user security management. The architecture follows a traditional mainframe pattern with VSAM files for data persistence, CICS for online transaction processing, and batch programs for periodic operations.

CardDemo demonstrates enterprise-grade application patterns including data migration utilities (`CBEXPORT`/`CBIMPORT`), multi-format reporting (`CBSTM03A`), and role-based access control. The system is designed for operational reliability with comprehensive error handling, data validation, and audit capabilities throughout all processing components.

## Subsystems/Modules

### 1. Online Transaction Processing (CICS)
**Purpose**: Interactive user interface for day-to-day operations
- **Authentication & Navigation**: `COSGN00C` (Signon), `COMEN01C` (User Menu), `COADM01C` (Admin Menu)
- **Account Management**: `COACTVWC` (Account View), `COACTUPC` (Account Update)
- **Card Management**: `COCRDLIC` (Card List), `COCRDSLC` (Card Select), `COCRDUPC` (Card Update)
- **Transaction Processing**: `COTRN00C` (Transaction List), `COTRN01C` (Transaction View), `COTRN02C` (Transaction Add), `COBIL00C` (Bill Payment)
- **User Administration**: `COUSR00C` (User List), `COUSR01C` (User Add), `COUSR02C` (User Update), `COUSR03C` (User Delete)
- **Reporting Interface**: `CORPT00C` (Report Submission)

### 2. Batch Processing
**Purpose**: Scheduled operations, data processing, and reporting
- **Data Migration**: `CBEXPORT` (Data Export), `CBIMPORT` (Data Import)
- **Transaction Processing**: `CBTRN01C` (Transaction Validation), `CBTRN02C` (Transaction Posting)
- **Account Operations**: `CBACT04C` (Interest Calculation), `CBACT01C` (Account Processing)
- **Reporting**: `CBSTM03A` (Statement Generation), `CBTRN03C` (Transaction Reporting)
- **Data Display Utilities**: `CBCUS01C` (Customer Display), `CBACT02C` (Card Display), `CBACT03C` (XREF Display)

### 3. Support Components
**Purpose**: Shared utilities and infrastructure
- **Date Validation**: `CSUTLDTC` (Date Validation Subroutine)
- **File Access**: `CBSTM03B` (File Access Subroutine)
- **System Utilities**: `COBSWAIT` (Wait Utility)

## Data Flow

### Core Data Stores
The system maintains several VSAM files that form the application database:
- **CUSTFILE/CUSTDAT**: Customer master data (personal information, contact details)
- **ACCTFILE/ACCTDAT**: Account master data (balances, limits, status, dates)
- **CARDFILE/CARDDAT**: Card details (card numbers, expiration, status)
- **XREFFILE/CXACAIX**: Cross-reference linking cards to accounts and customers
- **TRANSACT**: Transaction records (purchases, payments, adjustments)
- **TCATBAL**: Transaction category balances for interest calculation
- **USRSEC**: User security (credentials, permissions)

### Primary Processing Flows

#### 1. Transaction Processing Flow
```
Daily Transactions → CBTRN01C (Validation) → CBTRN02C (Posting) → 
    → Updates: TRANSACT (new transactions)
    → Updates: ACCTFILE (balance changes) 
    → Updates: TCATBAL (category balances)
    → Rejects: DALYREJS-FILE (failed transactions)
```

#### 2. Interest Calculation Flow
```
TCATBAL-FILE → CBACT04C (Interest Calc) → 
    → Creates: TRANSACT (interest transactions)
    → Updates: ACCTFILE (balance adjustments)
```

#### 3. Statement Generation Flow
```
TRANSACT + XREF + CUST + ACCT → CBSTM03A → 
    → Outputs: STMT-FILE (text statements)
    → Outputs: HTML-FILE (HTML statements)
```

#### 4. Online Operations Flow
```
User → COSGN00C (Auth) → Menu → Function Program → 
    → Reads/Writes: Appropriate VSAM files
    → Returns: Results to user screen
```

#### 5. Data Migration Flow
```
Source Files → CBEXPORT → Multi-record Export → 
    → CBIMPORT → Target Files (branch migration)
```

### Data Relationships
- **Card → Account → Customer**: Cards reference accounts via XREF, accounts reference customers
- **Transaction → Card**: Transactions reference card numbers, linked to accounts via XREF
- **User → Functions**: Users have types (regular/admin) determining accessible functions

## Program Index

| Program | Type | Description | Documentation |
|---------|------|-------------|---------------|
| **Batch Programs** | | | |
| `CBCUS01C` | Batch | Display customer records | [CBCUS01C.json](./final/programs/CBCUS01C.json) |
| `CBEXPORT` | Batch | Export normalized data to multi-record format | [CBEXPORT.json](./final/programs/CBEXPORT.json) |
| `CBSTM03A` | Batch | Generate customer account statements | [CBSTM03A.json](./final/programs/CBSTM03A.json) |
| `CBIMPORT` | Batch | Import multi-record data to normalized files | [CBIMPORT.json](./final/programs/CBIMPORT.json) |
| `CBACT03C` | Batch | Display account cross-reference data | [CBACT03C.json](./final/programs/CBACT03C.json) |
| `CBTRN02C` | Batch | Process and post daily transactions | [CBTRN02C.json](./final/programs/CBTRN02C.json) |
| `CBACT01C` | Batch | Account file processing and transformation | [CBACT01C.json](./final/programs/CBACT01C.json) |
| `CBTRN03C` | Batch | Generate transaction detail reports | [CBTRN03C.json](./final/programs/CBTRN03C.json) |
| `CBACT02C` | Batch | Display card data records | [CBACT02C.json](./final/programs/CBACT02C.json) |
| `CBTRN01C` | Batch | Validate daily transactions | [CBTRN01C.json](./final/programs/CBTRN01C.json) |
| `COBSWAIT` | Batch | Wait utility for specified time | [COBSWAIT.json](./final/programs/COBSWAIT.json) |
| `CBACT04C` | Batch | Calculate and post account interest | [CBACT04C.json](./final/programs/CBACT04C.json) |
| **Online (CICS) Programs** | | | |
| `COMEN01C` | CICS | Main menu for regular users | [COMEN01C.json](./final/programs/COMEN01C.json) |
| `COADM01C` | CICS | Administrative functions menu | [COADM01C.json](./final/programs/COADM01C.json) |
| `COCRDLIC` | CICS | Browse and select credit cards | [COCRDLIC.json](./final/programs/COCRDLIC.json) |
| `COACTUPC` | CICS | Update account and customer details | [COACTUPC.json](./final/programs/COACTUPC.json) |
| `COUSR03C` | CICS | Delete user security records | [COUSR03C.json](./final/programs/COUSR03C.json) |
| `COTRN02C` | CICS | Add new credit card transactions | [COTRN02C.json](./final/programs/COTRN02C.json) |
| `COACTVWC` | CICS | View account and customer details | [COACTVWC.json](./final/programs/COACTVWC.json) |
| `COUSR01C` | CICS | Add new users to security file | [COUSR01C.json](./final/programs/COUSR01C.json) |
| `COTRN00C` | CICS | List and select transactions | [COTRN00C.json](./final/programs/COTRN00C.json) |
| `CORPT00C` | CICS | Submit transaction report batch jobs | [CORPT00C.json](./final/programs/CORPT00C.json) |
| `COTRN01C` | CICS | View single transaction details | [COTRN01C.json](./final/programs/COTRN01C.json) |
| `COUSR00C` | CICS | List users for administration | [COUSR00C.json](./final/programs/COUSR00C.json) |
| `COBIL00C` | CICS | Process bill payments | [COBIL00C.json](./final/programs/COBIL00C.json) |
| `COUSR02C` | CICS | Update user security records | [COUSR02C.json](./final/programs/COUSR02C.json) |
| `COCRDUPC` | CICS | Update credit card details | [COCRDUPC.json](./final/programs/COCRDUPC.json) |
| `COSGN00C` | CICS | User authentication and signon | [COSGN00C.json](./final/programs/COSGN00C.json) |
| `COCRDSLC` | CICS | Select and display card details | [COCRDSLC.json](./final/programs/COCRDSLC.json) |
| **Subroutines** | | | |
| `CBSTM03B` | Subroutine | File access services for statement generation | [CBSTM03B.json](./final/programs/CBSTM03B.json) |
| `CSUTLDTC` | Subroutine | Date validation using CEEDAYS API | [CSUTLDTC.json](./final/programs/CSUTLDTC.json) |