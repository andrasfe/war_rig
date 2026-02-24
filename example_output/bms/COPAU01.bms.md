# COPAU01

**File**: `bms/COPAU01.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-02-24 17:38:07.760747

## Purpose

This BMS map defines the screen layout for the 'Pending Authorization Details' screen in the CardDemo application. It specifies the position, size, color, and attributes of various fields displayed on the screen, including transaction details, card information, authorization data, and merchant details. The map also includes function key assignments for navigation and fraud management.

**Business Context**: This screen is likely used to display and manage pending card authorization details within a card processing or fraud management system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| TRNNAME | IOType.CICS_MAP | Transaction name field. Input to display the transaction identifier. |
| PGMNAME | IOType.CICS_MAP | Program name field. Input to display the program identifier. |
| CARDNUM | IOType.CICS_MAP | Card number field. Input to display the card number. |
| AUTHDT | IOType.CICS_MAP | Authorization date field. Input to display the authorization date. |
| AUTHTM | IOType.CICS_MAP | Authorization time field. Input to display the authorization time. |
| AUTHRSP | IOType.CICS_MAP | Authorization response field. Input to display the authorization response code. |
| AUTHRSN | IOType.CICS_MAP | Authorization response reason field. Input to display the reason for the authorization response. |
| AUTHCD | IOType.CICS_MAP | Authorization code field. Input to display the authorization code. |
| AUTHAMT | IOType.CICS_MAP | Authorization amount field. Input to display the authorized amount. |
| POSEMD | IOType.CICS_MAP | POS entry mode field. Input to display the point-of-sale entry mode. |
| AUTHSRC | IOType.CICS_MAP | Authorization source field. Input to display the source of the authorization request. |
| MCCCD | IOType.CICS_MAP | MCC code field. Input to display the Merchant Category Code. |
| CRDEXP | IOType.CICS_MAP | Card expiration date field. Input to display the card's expiration date. |
| AUTHTYP | IOType.CICS_MAP | Authorization type field. Input to display the type of authorization. |
| TRNID | IOType.CICS_MAP | Transaction ID field. Input to display the transaction identifier. |
| AUTHMTC | IOType.CICS_MAP | Match status field. Input to display the match status of the transaction. |
| AUTHFRD | IOType.CICS_MAP | Fraud status field. Input to display the fraud status of the transaction. |
| MERNAME | IOType.CICS_MAP | Merchant name field. Input to display the name of the merchant. |
| MERID | IOType.CICS_MAP | Merchant ID field. Input to display the merchant identifier. |
| MERCITY | IOType.CICS_MAP | Merchant city field. Input to display the city of the merchant. |
| MERST | IOType.CICS_MAP | Merchant state field. Input to display the state of the merchant. |
| MERZIP | IOType.CICS_MAP | Merchant zip code field. Input to display the zip code of the merchant. |
| ERRMSG | IOType.CICS_MAP | Error message field. Input to display error messages. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| TRNNAME | IOType.CICS_MAP | Transaction name field. Output to display the transaction identifier. |
| PGMNAME | IOType.CICS_MAP | Program name field. Output to display the program identifier. |
| CARDNUM | IOType.CICS_MAP | Card number field. Output to display the card number. |
| AUTHDT | IOType.CICS_MAP | Authorization date field. Output to display the authorization date. |
| AUTHTM | IOType.CICS_MAP | Authorization time field. Output to display the authorization time. |
| AUTHRSP | IOType.CICS_MAP | Authorization response field. Output to display the authorization response code. |
| AUTHRSN | IOType.CICS_MAP | Authorization response reason field. Output to display the reason for the authorization response. |
| AUTHCD | IOType.CICS_MAP | Authorization code field. Output to display the authorization code. |
| AUTHAMT | IOType.CICS_MAP | Authorization amount field. Output to display the authorized amount. |
| POSEMD | IOType.CICS_MAP | POS entry mode field. Output to display the point-of-sale entry mode. |
| AUTHSRC | IOType.CICS_MAP | Authorization source field. Output to display the source of the authorization request. |
| MCCCD | IOType.CICS_MAP | MCC code field. Output to display the Merchant Category Code. |
| CRDEXP | IOType.CICS_MAP | Card expiration date field. Output to display the card's expiration date. |
| AUTHTYP | IOType.CICS_MAP | Authorization type field. Output to display the type of authorization. |
| TRNID | IOType.CICS_MAP | Transaction ID field. Output to display the transaction identifier. |
| AUTHMTC | IOType.CICS_MAP | Match status field. Output to display the match status of the transaction. |
| AUTHFRD | IOType.CICS_MAP | Fraud status field. Output to display the fraud status of the transaction. |
| MERNAME | IOType.CICS_MAP | Merchant name field. Output to display the name of the merchant. |
| MERID | IOType.CICS_MAP | Merchant ID field. Output to display the merchant identifier. |
| MERCITY | IOType.CICS_MAP | Merchant city field. Output to display the city of the merchant. |
| MERST | IOType.CICS_MAP | Merchant state field. Output to display the state of the merchant. |
| MERZIP | IOType.CICS_MAP | Merchant zip code field. Output to display the zip code of the merchant. |
| ERRMSG | IOType.CICS_MAP | Error message field. Output to display error messages. |

## Paragraphs/Procedures

### ~~COPAU01~~ (Dead Code)
*Artifact 'COPAU01' (map) is never referenced by any other artifact in the dependency graph*

### ~~COPAU1A~~ (Dead Code)
*Screen/Map 'COPAU1A' is never sent to or received from by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU01 | map | 19 | Artifact 'COPAU01' (map) is never referenced by any other artifact in the dependency graph |
| COPAU1A | screen | 26 | Screen/Map 'COPAU1A' is never sent to or received from by any program |
