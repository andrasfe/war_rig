```cobol
       5300-READ-CUST-RECORD.                                           
           DISPLAY 'SPECTER-TRACE:5300-READ-CUST-RECORD'.
      *
            MOVE WS-CARD-RID-CUST-ID   TO WS-CARD-RID-CUST-ID
                                                                        
      *    EXEC CICS READ                                               
      *         DATASET   (WS-CUSTFILENAME)                             
      *         RIDFLD    (WS-CARD-RID-CUST-ID-X)                       
      *         KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)             
      *         INTO      (CUSTOMER-RECORD)                             
      *         LENGTH    (LENGTH OF CUSTOMER-RECORD)                   
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
                  SET FOUND-CUST-IN-MSTR     TO TRUE                    
                WHEN 13
                  SET NFOUND-CUST-IN-MSTR    TO TRUE                    
                                                                        
                  MOVE 'A003'                TO ERR-LOCATION            
                  SET  ERR-WARNING           TO TRUE                    
                  SET  ERR-APP               TO TRUE                    
                  MOVE 'CUST NOT FOUND IN XREF'                         
                                             TO ERR-MESSAGE             
                  MOVE WS-CARD-RID-CUST-ID   TO ERR-EVENT-KEY           
           DISPLAY 'SPECTER-CALL:FROM=5300-READ-CUST-RECORD:TO=9500-LO'
                  PERFORM 9500-LOG-ERROR                                
      *                                                                 
               WHEN OTHER                                               
                  MOVE 'C003'                TO ERR-LOCATION            
                  SET  ERR-CRITICAL          TO TRUE                    
                  SET  ERR-CICS              TO TRUE                    
                  MOVE WS-RESP-CD            TO WS-CODE-DISPLAY         
                  MOVE WS-CODE-DISPLAY       TO ERR-CODE-1              
                  MOVE WS-REAS-CD            TO WS-CODE-DISPLAY         
                  MOVE WS-CODE-DISPLAY       TO ERR-CODE-2              
                  MOVE 'FAILED TO READ CUST FILE'                       
                                             TO ERR-MESSAGE             
                  MOVE WS-CARD-RID-CUST-ID   TO ERR-EVENT-KEY           
           DISPLAY 'SPECTER-CALL:FROM=5300-READ-CUST-RECORD:TO=9500-LO'
                  PERFORM 9500-LOG-ERROR                                
           END-EVALUATE                                                 
           .                                                            
```
