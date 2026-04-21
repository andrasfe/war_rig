```cobol
       9100-CLOSE-REQUEST-QUEUE.                                        
           DISPLAY 'SPECTER-TRACE:9100-CLOSE-REQUEST-QUEUE'.
      *
           IF WS-REQUEST-MQ-OPEN                                        
      *       CALL 'MQCLOSE' USING W01-HCONN-REQUEST                    
      *                         W01-HOBJ-REQUEST                        
      *                         0
      *                         WS-COMPCODE                             
      *                         WS-REASON                               
      *       END-CALL                                                  
           DISPLAY 'SPECTER-MOCK:CALL:MQCLOSE'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
           MOVE MOCK-NUM-STATUS TO RETURN-CODE
      *                                                                 
               IF WS-COMPCODE = 0
                 SET WS-REQUEST-MQ-CLSE TO TRUE                         
              ELSE                                                      
                 MOVE 'M005'                TO ERR-LOCATION             
                 SET  ERR-WARNING           TO TRUE                     
                 SET  ERR-MQ                TO TRUE                     
                 MOVE WS-COMPCODE           TO WS-CODE-DISPLAY          
                 MOVE WS-CODE-DISPLAY       TO ERR-CODE-1               
                 MOVE WS-REASON             TO WS-CODE-DISPLAY          
                 MOVE WS-CODE-DISPLAY       TO ERR-CODE-2               
                 MOVE 'FAILED TO CLOSE REQUEST MQ'                      
                                            TO ERR-MESSAGE              
           DISPLAY 'SPECTER-CALL:FROM=9100-CLOSE-REQUEST-QUEUE:TO=9500'
                 PERFORM 9500-LOG-ERROR                                 
              END-IF                                                    
           END-IF.                                                      
```
