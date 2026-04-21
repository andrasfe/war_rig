```cobol
       8400-UPDATE-SUMMARY.                                             
           DISPLAY 'SPECTER-TRACE:8400-UPDATE-SUMMARY'.
      *
           IF NFOUND-PAUT-SMRY-SEG                                      
              INITIALIZE PENDING-AUTH-SUMMARY                           
                REPLACING NUMERIC DATA BY ZERO                          
                                                                        
              MOVE XREF-ACCT-ID             TO PA-ACCT-ID               
                MOVE WS-CARD-RID-CUST-ID   TO PA-CUST-ID
                                                                        
           END-IF                                                       
                                                                        
             MOVE WS-ACCT-CREDIT-LIMIT        TO PA-CREDIT-LIMIT
             MOVE WS-ACCT-CASH-LIMIT         TO PA-CASH-LIMIT
                                                                        
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
      *       EXEC DLI REPL USING PCB(PAUT-PCB-NUM)                     
      *            SEGMENT (PAUTSUM0)                                   
      *            FROM (PENDING-AUTH-SUMMARY)                          
      *       END-EXEC                                                  
           DISPLAY 'SPECTER-MOCK:DLI-REPL'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '  ' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
           MOVE MOCK-ALPHA-STATUS TO DIBSTAT
           ELSE                                                         
      *       EXEC DLI ISRT USING PCB(PAUT-PCB-NUM)                     
      *            SEGMENT (PAUTSUM0)                                   
      *            FROM (PENDING-AUTH-SUMMARY)                          
      *       END-EXEC                                                  
           DISPLAY 'SPECTER-MOCK:DLI-ISRT'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '  ' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
           MOVE MOCK-ALPHA-STATUS TO DIBSTAT
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
           DISPLAY 'SPECTER-CALL:FROM=8400-UPDATE-SUMMARY:TO=9500-LOG-'
             PERFORM 9500-LOG-ERROR                                     
           END-IF                                                       
           .                                                            
```
