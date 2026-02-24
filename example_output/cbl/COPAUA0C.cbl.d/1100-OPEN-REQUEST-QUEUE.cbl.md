```cobol
       1100-OPEN-REQUEST-QUEUE.                                                 
      *                                                                         
           MOVE MQOT-Q             TO MQOD-OBJECTTYPE OF MQM-OD-REQUEST         
           MOVE WS-REQUEST-QNAME   TO MQOD-OBJECTNAME OF MQM-OD-REQUEST         
      *                                                                         
           COMPUTE WS-OPTIONS = MQOO-INPUT-SHARED                               
      *                                                                         
           CALL 'MQOPEN' USING W01-HCONN-REQUEST                                
                               MQM-OD-REQUEST                                   
                               WS-OPTIONS                                       
                               W01-HOBJ-REQUEST                                 
                               WS-COMPCODE                                      
                               WS-REASON                                        
           END-CALL                                                             
      *                                                                         
           IF WS-COMPCODE = MQCC-OK                                             
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
              PERFORM 9500-LOG-ERROR                                            
           END-IF                                                               
           .                                                                    
      *                                                                         
       1100-EXIT.                                                               
```
