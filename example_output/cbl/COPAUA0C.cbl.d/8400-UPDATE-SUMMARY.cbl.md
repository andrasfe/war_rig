```cobol
       8400-UPDATE-SUMMARY.                                                     
      * ------------------------------------------------------------- *         
      *                                                                         
           IF NFOUND-PAUT-SMRY-SEG                                              
              INITIALIZE PENDING-AUTH-SUMMARY                                   
                REPLACING NUMERIC DATA BY ZERO                                  
                                                                                
              MOVE XREF-ACCT-ID             TO PA-ACCT-ID                       
              MOVE XREF-CUST-ID             TO PA-CUST-ID                       
                                                                                
           END-IF                                                               
                                                                                
           MOVE ACCT-CREDIT-LIMIT           TO PA-CREDIT-LIMIT                  
           MOVE ACCT-CASH-CREDIT-LIMIT      TO PA-CASH-LIMIT                    
                                                                                
           IF AUTH-RESP-APPROVED                                                
              ADD 1                         TO PA-APPROVED-AUTH-CNT             
              ADD WS-APPROVED-AMT           TO PA-APPROVED-AUTH-AMT             
                                                                                
              ADD WS-APPROVED-AMT           TO PA-CREDIT-BALANCE                
              MOVE 0                        TO PA-CASH-BALANCE                  
           ELSE                                                                 
              ADD 1                         TO PA-DECLINED-AUTH-CNT             
              ADD PA-TRANSACTION-AMT        TO PA-DECLINED-AUTH-AMT             
           END-IF                                                               
                                                                                
           IF FOUND-PAUT-SMRY-SEG                                               
              EXEC DLI REPL USING PCB(PAUT-PCB-NUM)                             
                   SEGMENT (PAUTSUM0)                                           
                   FROM (PENDING-AUTH-SUMMARY)                                  
              END-EXEC                                                          
           ELSE                                                                 
              EXEC DLI ISRT USING PCB(PAUT-PCB-NUM)                             
                   SEGMENT (PAUTSUM0)                                           
                   FROM (PENDING-AUTH-SUMMARY)                                  
              END-EXEC                                                          
           END-IF                                                               
           MOVE DIBSTAT                     TO IMS-RETURN-CODE                  
                                                                                
           IF STATUS-OK                                                         
             CONTINUE                                                           
           ELSE                                                                 
             MOVE 'I003'                    TO ERR-LOCATION                     
             SET  ERR-CRITICAL              TO TRUE                             
             SET  ERR-IMS                   TO TRUE                             
             MOVE IMS-RETURN-CODE           TO ERR-CODE-1                       
             MOVE 'IMS UPDATE SUMRY FAILED' TO ERR-MESSAGE                      
             MOVE PA-CARD-NUM               TO ERR-EVENT-KEY                    
             PERFORM 9500-LOG-ERROR                                             
```
