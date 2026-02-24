```cobol
       7100-SEND-RESPONSE.                                                      
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE MQOT-Q               TO MQOD-OBJECTTYPE OF MQM-OD-REPLY         
           MOVE WS-REPLY-QNAME       TO MQOD-OBJECTNAME OF MQM-OD-REPLY         
      *                                                                         
           MOVE MQMT-REPLY           TO MQMD-MSGTYPE     OF MQM-MD-REPLY        
           MOVE WS-SAVE-CORRELID     TO MQMD-CORRELID    OF MQM-MD-REPLY        
           MOVE MQMI-NONE            TO MQMD-MSGID       OF MQM-MD-REPLY        
           MOVE SPACES               TO MQMD-REPLYTOQ    OF MQM-MD-REPLY        
           MOVE SPACES               TO MQMD-REPLYTOQMGR OF MQM-MD-REPLY        
           MOVE MQPER-NOT-PERSISTENT TO MQMD-PERSISTENCE OF MQM-MD-REPLY        
           MOVE 50                   TO MQMD-EXPIRY      OF MQM-MD-REPLY        
           MOVE MQFMT-STRING         TO MQMD-FORMAT      OF MQM-MD-REPLY        
                                                                                
           COMPUTE MQPMO-OPTIONS     =  MQPMO-NO-SYNCPOINT +                    
                                        MQPMO-DEFAULT-CONTEXT                   
                                                                                
           MOVE WS-RESP-LENGTH       TO W02-BUFFLEN                             
      *                                                                         
           CALL 'MQPUT1' USING W02-HCONN-REPLY                                  
                               MQM-OD-REPLY                                     
                               MQM-MD-REPLY                                     
                               MQM-PUT-MESSAGE-OPTIONS                          
                               W02-BUFFLEN                                      
                               W02-PUT-BUFFER                                   
                               WS-COMPCODE                                      
                               WS-REASON                                        
           END-CALL                                                             
           IF WS-COMPCODE NOT = MQCC-OK                                         
              MOVE 'M004'                TO ERR-LOCATION                        
              SET  ERR-CRITICAL          TO TRUE                                
              SET  ERR-MQ                TO TRUE                                
              MOVE WS-COMPCODE           TO WS-CODE-DISPLAY                     
              MOVE WS-CODE-DISPLAY       TO ERR-CODE-1                          
              MOVE WS-REASON             TO WS-CODE-DISPLAY                     
              MOVE WS-CODE-DISPLAY       TO ERR-CODE-2                          
```
