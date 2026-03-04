```cobol
             88 DATABASE-UNAVAILABLE         VALUE 'BA'.                        
             88 PSB-SCHEDULED-MORE-THAN-ONCE VALUE 'TC'.                        
             88 COULD-NOT-SCHEDULE-PSB       VALUE 'TE'.                        
             88 RETRY-CONDITION              VALUE 'BA', 'FH', 'TE'.            
          05 WS-IMS-PSB-SCHD-FLG             PIC X(1).                          
             88  IMS-PSB-SCHD                VALUE 'Y'.                         
             88  IMS-PSB-NOT-SCHD            VALUE 'N'.                         
                                                                                
                                                                                
       01  WS-SWITCHES.                                                         
           05 WS-XREF-READ-FLG           PIC X(1).                              
              88 ACCT-NFOUND-XREF                  VALUE 'N'.                   
              88 ACCT-FOUND-XREF                   VALUE 'Y'.                   
           05 WS-ACCT-MASTER-READ-FLG    PIC X(1).                              
              88 FOUND-ACCT-IN-MSTR                VALUE 'Y'.                   
              88 NFOUND-ACCT-IN-MSTR               VALUE 'N'.                   
           05 WS-CUST-MASTER-READ-FLG    PIC X(1).                              
              88 FOUND-CUST-IN-MSTR                VALUE 'Y'.                   
              88 NFOUND-CUST-IN-MSTR               VALUE 'N'.                   
           05 WS-PAUT-SMRY-SEG-FLG       PIC X(1).                              
              88 FOUND-PAUT-SMRY-SEG               VALUE 'Y'.                   
              88 NFOUND-PAUT-SMRY-SEG              VALUE 'N'.                   
           05 WS-ERR-FLG                 PIC X(1)  VALUE 'N'.                   
              88 ERR-FLG-ON                        VALUE 'Y'.                   
              88 ERR-FLG-OFF                       VALUE 'N'.                   
           05 WS-AUTHS-EOF               PIC X(1)  VALUE 'N'.                   
              88 AUTHS-EOF                         VALUE 'Y'.                   
              88 AUTHS-NOT-EOF                     VALUE 'N'.                   
           05 WS-SEND-ERASE-FLG          PIC X(1)  VALUE 'Y'.                   
              88 SEND-ERASE-YES                    VALUE 'Y'.                   
              88 SEND-ERASE-NO                     VALUE 'N'.                   
                                                                                
       COPY COCOM01Y.                                                           
          05 CDEMO-CPVS-INFO.                                                   
             10 CDEMO-CPVS-PAU-SEL-FLG     PIC X(01).                           
             10 CDEMO-CPVS-PAU-SELECTED    PIC X(08).                           
             10 CDEMO-CPVS-PAUKEY-PREV-PG  PIC X(08) OCCURS 20 TIMES.           
             10 CDEMO-CPVS-PAUKEY-LAST     PIC X(08).                           
             10 CDEMO-CPVS-PAGE-NUM        PIC S9(04) COMP.                     
             10 CDEMO-CPVS-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.                 
                88 NEXT-PAGE-YES                     VALUE 'Y'.                 
                88 NEXT-PAGE-NO                      VALUE 'N'.                 
             10 CDEMO-CPVS-AUTH-KEYS       PIC X(08) OCCURS 5 TIMES.            
                                                                                
      *BMS Copybook
       COPY COPAU00.

      *Screen Titles
       COPY COTTL01Y.

      *Current Date
       COPY CSDAT01Y.

      *Common Messages
       COPY CSMSG01Y.

      *Abend Variables
       COPY CSMSG02Y.

      *ACCOUNT RECORD LAYOUT
       COPY CVACT01Y.

      *CUSTOMER RECORD LAYOUT
       COPY CVACT02Y.

      *CARD XREF LAYOUT
       COPY CVACT03Y.

      *CUSTOMER LAYOUT
       COPY CVCUS01Y.

      *----------------------------------------------------------------*
      *  IMS SEGMENT LAYOUT
      *----------------------------------------------------------------*

      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT
       01 PENDING-AUTH-SUMMARY.
       COPY CIPAUSMY.
```
