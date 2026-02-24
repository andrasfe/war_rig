```cobol
      ******************************************************************
      * Program     : COPAUA0C.CBL
      * Application : CardDemo - Authorization Module
      * Type        : CICS COBOL IMS MQ Program
      * Function    : Card Authorization Decision Program
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
       PROGRAM-ID. COPAUA0C.                                                    
       AUTHOR.     SOUMA GHOSH.                                                 
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
                                                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01 WS-VARIABLES.                                                         
         05 WS-PGM-AUTH                PIC X(08)  VALUE 'COPAUA0C'.             
         05 WS-CICS-TRANID             PIC X(04)  VALUE 'CP00'.                 
         05 WS-ACCTFILENAME            PIC X(8)   VALUE 'ACCTDAT '.             
         05 WS-CUSTFILENAME            PIC X(8)   VALUE 'CUSTDAT '.             
         05 WS-CARDFILENAME            PIC X(8)   VALUE 'CARDDAT '.             
         05 WS-CARDFILENAME-ACCT-PATH  PIC X(8)   VALUE 'CARDAIX '.             
         05 WS-CCXREF-FILE             PIC X(08)  VALUE 'CCXREF  '.             
         05 WS-REQSTS-PROCESS-LIMIT    PIC S9(4)  COMP VALUE 500.               
                                                                                
         05 WS-MSG-PROCESSED           PIC S9(4)  COMP VALUE ZERO.              
         05 WS-REQUEST-QNAME           PIC X(48).                               
```
