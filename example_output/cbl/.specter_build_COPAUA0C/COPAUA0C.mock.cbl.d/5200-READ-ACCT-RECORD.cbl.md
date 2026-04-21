```cobol
       5200-READ-ACCT-RECORD.                                           
           DISPLAY 'SPECTER-TRACE:5200-READ-ACCT-RECORD'.
      *
            MOVE WS-CARD-RID-ACCT-ID   TO WS-CARD-RID-ACCT-ID
                                                                        
      *    EXEC CICS READ                                               
      *         DATASET   (WS-ACCTFILENAME)                             
      *         RIDFLD    (WS-CARD-RID-ACCT-ID-X)                       
      *         KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)             
      *         INTO      (ACCOUNT-RECORD)                              
      *         LENGTH    (LENGTH OF ACCOUNT-RECORD)                    
      *         RESP      (WS-RESP-CD)                                  
      *         RESP2     (WS-REAS-CD)                                  
      *    END-EXEC                                                     
           DISPLAY 'SPECTER-MOCK:CICS-READ'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '00' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
           MOVE MOCK-NUM-STATUS TO WS-RESP-CD
           MOVE 0 TO WS-REAS-CD
                                                                        
           EVALUATE WS-RESP-CD                                          
                WHEN 0
                  SET FOUND-ACCT-IN-MSTR     TO TRUE                    
                WHEN 13
                  SET NFOUND-ACCT-IN-MSTR    TO TRUE                    
                                                                        
                  MOVE 'A002'                TO ERR-LOCATION            
                  SET  ERR-WARNING           TO TRUE                    
                  SET  ERR-APP               TO TRUE                    
                  MOVE 'ACCT NOT FOUND IN XREF'                         
                                             TO ERR-MESSAGE             
                  MOVE WS-CARD-RID-ACCT-ID-X TO ERR-EVENT-KEY           
           DISPLAY 'SPECTER-CALL:FROM=5200-READ-ACCT-RECORD:TO=9500-LO'
                  PERFORM 9500-LOG-ERROR                                
      *                                                                 
               WHEN OTHER                                               
                  MOVE 'C002'                TO ERR-LOCATION            
                  SET  ERR-CRITICAL          TO TRUE                    
                  SET  ERR-CICS              TO TRUE                    
                  MOVE WS-RESP-CD            TO WS-CODE-DISPLAY         
                  MOVE WS-CODE-DISPLAY       TO ERR-CODE-1              
                  MOVE WS-REAS-CD            TO WS-CODE-DISPLAY         
                  MOVE WS-CODE-DISPLAY       TO ERR-CODE-2              
                  MOVE 'FAILED TO READ ACCT FILE'                       
                                             TO ERR-MESSAGE             
                  MOVE WS-CARD-RID-ACCT-ID-X TO ERR-EVENT-KEY           
           DISPLAY 'SPECTER-CALL:FROM=5200-READ-ACCT-RECORD:TO=9500-LO'
                  PERFORM 9500-LOG-ERROR                                
           END-EVALUATE                                                 
           .                                                            
```
