```cobol
      ******************************************************************
      * Program     : COPAUS0C.CBL
      * Application : CardDemo - Authorization Module
      * Type        : CICS COBOL IMS BMS Program
      * Function    : Summary View of Authoriation Messages
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
       PROGRAM-ID. COPAUS0C.                                                    
       AUTHOR.     AWS.                                                         
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
                                                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01 WS-VARIABLES.                                                         
         05 WS-PGM-AUTH-SMRY           PIC X(08) VALUE 'COPAUS0C'.              
         05 WS-PGM-AUTH-DTL            PIC X(08) VALUE 'COPAUS1C'.              
         05 WS-PGM-MENU                PIC X(08) VALUE 'COMEN01C'.              
         05 WS-CICS-TRANID             PIC X(04) VALUE 'CPVS'.                  
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.                  
         05 WS-ACCTFILENAME            PIC X(8)  VALUE 'ACCTDAT '.              
         05 WS-CUSTFILENAME            PIC X(8)  VALUE 'CUSTDAT '.              
         05 WS-CARDFILENAME            PIC X(8)  VALUE 'CARDDAT '.              
         05 WS-CARDXREFNAME-ACCT-PATH  PIC X(8)  VALUE 'CXACAIX '.              
         05 WS-CCXREF-FILE             PIC X(08) VALUE 'CCXREF  '.              
                                                                                
         05 WS-ACCT-ID                 PIC  X(11).                              
         05 WS-AUTH-KEY-SAVE           PIC  X(08).                              
         05 WS-AUTH-APRV-STAT          PIC  X(01).                              
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.             
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.             
         05 WS-RESP-CD-DIS             PIC  9(09).                              
         05 WS-REAS-CD-DIS             PIC  9(09).                              
         05 WS-REC-COUNT               PIC S9(04) COMP VALUE ZEROS.             
         05 WS-IDX                     PIC S9(04) COMP VALUE ZEROS.             
         05 WS-PAGE-NUM                PIC S9(04) COMP VALUE ZEROS.             
                                                                                
         05 WS-AUTH-AMT                PIC -zzzzzzz9.99.                        
```
