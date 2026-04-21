```cobol
       1100-OPEN-REQUEST-QUEUE.                                         
           DISPLAY 'SPECTER-TRACE:1100-OPEN-REQUEST-QUEUE'.
      *                                                                 
             MOVE 1                  TO MQOD-OBJECTTYPE
             MOVE WS-REQUEST-QNAME   TO MQOD-OBJECTNAME
      *                                                                 
            COMPUTE WS-OPTIONS = 2048
      *                                                                 
      *    CALL 'MQOPEN' USING W01-HCONN-REQUEST                        
      *                        MQM-OD-REQUEST                           
      *                        WS-OPTIONS                               
      *                        W01-HOBJ-REQUEST                         
      *                        WS-COMPCODE                              
      *                        WS-REASON                                
      *    END-CALL                                                     
           DISPLAY 'SPECTER-MOCK:CALL:MQOPEN'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
           MOVE MOCK-NUM-STATUS TO RETURN-CODE
      *                                                                 
            IF WS-COMPCODE = 0
              SET WS-REQUEST-MQ-OPEN TO TRUE                            
           ELSE                                                         
              MOVE 'M001'          TO ERR-LOCATION                      
              SET  ERR-CRITICAL    TO TRUE                              
              SET  ERR-MQ          TO TRUE                              
              MOVE WS-COMPCODE     TO WS-CODE-DISPLAY                   
              MOVE WS-CODE-DISPLAY TO ERR-CODE-1                        
              MOVE WS-REASON       TO WS-CODE-DISPLAY                   
              MOVE WS-CODE-DISPLAY TO ERR-CODE-2                        
              MOVE 'REQ MQ OPEN ERROR'                                  
                                   TO ERR-MESSAGE                       
           DISPLAY 'SPECTER-CALL:FROM=1100-OPEN-REQUEST-QUEUE:TO=9500-'
              PERFORM 9500-LOG-ERROR                                    
           END-IF                                                       
           .                                                            
```
