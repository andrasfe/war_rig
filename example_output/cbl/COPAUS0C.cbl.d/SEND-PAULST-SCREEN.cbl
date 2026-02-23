           10  WS-CARD-RID-ACCT-ID-X REDEFINES                                  
                  WS-CARD-RID-ACCT-ID              PIC X(11).                   
                                                                                
       01 WS-IMS-VARIABLES.
          05 PSB-NAME                        PIC X(8) VALUE 'PSBPAUTB'.
          05 PCB-OFFSET.
             10 PAUT-PCB-NUM                 PIC S9(4) COMP VALUE +1.
          05 IMS-RETURN-CODE                 PIC X(02).
             88 STATUS-OK                    VALUE '  ', 'FW'.                  
             88 SEGMENT-NOT-FOUND            VALUE 'GE'.                        
             88 DUPLICATE-SEGMENT-FOUND      VALUE 'II'.                        
             88 WRONG-PARENTAGE              VALUE 'GP'.                        
             88 END-OF-DB                    VALUE 'GB'.                        
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
