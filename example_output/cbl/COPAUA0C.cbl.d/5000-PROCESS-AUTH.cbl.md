```cobol
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
                                                                                
       01  W01-HCONN-REQUEST           PIC S9(9) BINARY VALUE ZERO.             
       01  W01-HOBJ-REQUEST            PIC S9(9) BINARY.                        
       01  W01-BUFFLEN                 PIC S9(9) BINARY.                        
       01  W01-DATALEN                 PIC S9(9) BINARY.                        
       01  W01-GET-BUFFER              PIC X(500).                              
                                                                                
       01  W02-HCONN-REPLY             PIC S9(9) BINARY VALUE ZERO.             
       01  W02-BUFFLEN                 PIC S9(9) BINARY.                        
       01  W02-DATALEN                 PIC S9(9) BINARY.                        
       01  W02-PUT-BUFFER              PIC X(200).                              
                                                                                
       01  WS-SWITCHES.                                                         
           05 WS-AUTH-RESP-FLG         PIC X(01).                               
              88 AUTH-RESP-APPROVED    VALUE 'A'.                               
              88 AUTH-RESP-DECLINED    VALUE 'D'.                               
           05 WS-MSG-LOOP-FLG          PIC X(01) VALUE 'N'.                     
```
