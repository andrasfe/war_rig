```cobol
       9100-CLOSE-REQUEST-QUEUE.                                                
      * ------------------------------------------------------------ *          
           IF WS-REQUEST-MQ-OPEN                                                
              CALL 'MQCLOSE' USING W01-HCONN-REQUEST                            
                                W01-HOBJ-REQUEST                                
                                MQCO-NONE                                       
                                WS-COMPCODE                                     
                                WS-REASON                                       
              END-CALL                                                          
      *                                                                         
              IF WS-COMPCODE = MQCC-OK                                          
                 SET WS-REQUEST-MQ-CLSE TO TRUE                                 
              ELSE                                                              
                 MOVE 'M005'                TO ERR-LOCATION                     
                 SET  ERR-WARNING           TO TRUE                             
                 SET  ERR-MQ                TO TRUE                             
```
