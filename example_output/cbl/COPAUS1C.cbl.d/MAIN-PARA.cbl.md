```cobol
      * Application : CardDemo - Authorization Module
      * Type        : CICS COBOL IMS BMS Program
      * Function    : Detail View of Authorization Message
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.
      * All Rights Reserved.
      *
      * Licensed under the Apache License, Version 2.0 (the "License").
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at
      *
      *    http://www.apache.org/licenses/LICENSE-2.0
      *
      * Unless required by applicable law or agreed to in writing,
      * software distributed under the License is distributed on an
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
      * either express or implied. See the License for the specific
      * language governing permissions and limitations under the License
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPAUS1C.                                                    
       AUTHOR.     AWS.                                                         
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
                                                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01 WS-VARIABLES.                                                         
         05 WS-PGM-AUTH-DTL            PIC X(08) VALUE 'COPAUS1C'.              
         05 WS-PGM-AUTH-SMRY           PIC X(08) VALUE 'COPAUS0C'.              
         05 WS-PGM-AUTH-FRAUD          PIC X(08) VALUE 'COPAUS2C'.              
         05 WS-CICS-TRANID             PIC X(04) VALUE 'CPVD'.                  
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.                  
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.                     
           88 ERR-FLG-ON                         VALUE 'Y'.                     
           88 ERR-FLG-OFF                        VALUE 'N'.                     
         05 WS-AUTHS-EOF               PIC X(01) VALUE 'N'.                     
           88 AUTHS-EOF                          VALUE 'Y'.                     
           88 AUTHS-NOT-EOF                      VALUE 'N'.                     
         05 WS-SEND-ERASE-FLG          PIC X(01) VALUE 'Y'.                     
           88 SEND-ERASE-YES                     VALUE 'Y'.                     
           88 SEND-ERASE-NO                      VALUE 'N'.                     
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.             
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.             
                                                                                
         05 WS-ACCT-ID                 PIC  9(11).                              
         05 WS-AUTH-KEY                PIC  X(08).                              
         05 WS-AUTH-AMT                PIC -zzzzzzz9.99.                        
```
