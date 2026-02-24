```cobol
         05 WS-TRIGGER-DATA            PIC X(64).                               
                                                                                
      ******************************************************************        
      *      File and data Handling                                             
      ******************************************************************        
         05 WS-XREF-RID.                                                        
           10  WS-CARD-RID-CARDNUM                 PIC X(16).                   
           10  WS-CARD-RID-CUST-ID                 PIC 9(09).                   
           10  WS-CARD-RID-CUST-ID-X REDEFINES                                  
                  WS-CARD-RID-CUST-ID              PIC X(09).                   
           10  WS-CARD-RID-ACCT-ID                 PIC 9(11).                   
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
              88 WS-LOOP-END           VALUE 'E'.                               
           05 WS-MSG-AVAILABLE-FLG     PIC X(01) VALUE 'M'.                     
              88 NO-MORE-MSG-AVAILABLE VALUE 'N'.                               
              88 MORE-MSG-AVAILABLE    VALUE 'M'.                               
           05 WS-REQUEST-MQ-FLG        PIC X(01) VALUE 'C'.                     
              88 WS-REQUEST-MQ-OPEN    VALUE 'O'.                               
              88 WS-REQUEST-MQ-CLSE    VALUE 'C'.                               
           05 WS-REPLY-MQ-FLG          PIC X(01) VALUE 'C'.                     
              88 WS-REPLY-MQ-OPEN      VALUE 'O'.                               
              88 WS-REPLY-MQ-CLSE      VALUE 'C'.                               
           05 WS-XREF-READ-FLG         PIC X(1).                                
              88 CARD-NFOUND-XREF      VALUE 'N'.                               
              88 CARD-FOUND-XREF       VALUE 'Y'.                               
           05 WS-ACCT-MASTER-READ-FLG PIC X(1).                                 
              88 FOUND-ACCT-IN-MSTR    VALUE 'Y'.                               
              88 NFOUND-ACCT-IN-MSTR   VALUE 'N'.                               
           05 WS-CUST-MASTER-READ-FLG PIC X(1).                                 
              88 FOUND-CUST-IN-MSTR    VALUE 'Y'.                               
              88 NFOUND-CUST-IN-MSTR   VALUE 'N'.                               
           05 WS-PAUT-SMRY-SEG-FLG     PIC X(1).                                
              88 FOUND-PAUT-SMRY-SEG   VALUE 'Y'.                               
              88 NFOUND-PAUT-SMRY-SEG  VALUE 'N'.                               
           05 WS-DECLINE-FLG           PIC X(1).                                
              88 APPROVE-AUTH          VALUE 'A'.                               
              88 DECLINE-AUTH          VALUE 'D'.                               
           05 WS-DECLINE-REASON-FLG    PIC X(1).                                
              88 INSUFFICIENT-FUND     VALUE 'I'.                               
              88 CARD-NOT-ACTIVE       VALUE 'A'.                               
              88 ACCOUNT-CLOSED        VALUE 'C'.                               
              88 CARD-FRAUD            VALUE 'F'.                               
              88 MERCHANT-FRAUD        VALUE 'M'.                               
                                                                                
```
