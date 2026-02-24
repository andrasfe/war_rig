```cobol
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
                                                                                
                                                                                
       01  MQM-OD-REQUEST.                                                      
           COPY CMQODV.                                                         
                                                                                
       01  MQM-MD-REQUEST.                                                      
           COPY CMQMDV.                                                         
                                                                                
       01  MQM-OD-REPLY.                                                        
           COPY CMQODV.                                                         
                                                                                
       01  MQM-MD-REPLY.                                                        
           COPY CMQMDV.                                                         
                                                                                
       01  MQM-CONSTANTS.                                                       
           COPY CMQV.                                                           
```
