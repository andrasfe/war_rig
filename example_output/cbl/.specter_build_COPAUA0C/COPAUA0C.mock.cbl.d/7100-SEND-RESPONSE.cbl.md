```cobol
       7100-SEND-RESPONSE.                                              
           DISPLAY 'SPECTER-TRACE:7100-SEND-RESPONSE'.
      *
      *     MOVE 1                    TO MQOD-OBJECTTYPE OF MQM-OD-REPLY
      *    MOVE WS-REPLY-QNAME       TO MQOD-OBJECTNAME OF MQM-OD-REPLY 
      *                                                                 
             MOVE 2                    TO MQMD-MSGTYPE
      *    MOVE WS-SAVE-CORRELID     TO MQMD-CORRELID    OF MQM-MD-REPLY
      *     MOVE 0                    TO MQMD-MSGID       OF MQM-MD-REPLY
      *    MOVE SPACES               TO MQMD-REPLYTOQ    OF MQM-MD-REPLY
             MOVE SPACES               TO MQMD-REPLYTOQMGR
             MOVE 0                    TO MQMD-PERSISTENCE
             MOVE 50                   TO MQMD-EXPIRY
      *     MOVE 'MQSTR   '           TO MQMD-FORMAT      OF MQM-MD-REPLY
                                                                        
             COMPUTE MQPMO-OPTIONS = 2 +
                                         4
                                                                        
           MOVE WS-RESP-LENGTH       TO W02-BUFFLEN                     
      *                                                                 
      *    CALL 'MQPUT1' USING W02-HCONN-REPLY                          
      *                        MQM-OD-REPLY                             
      *                        MQM-MD-REPLY                             
      *                        MQM-PUT-MESSAGE-OPTIONS                  
      *                        W02-BUFFLEN                              
      *                        W02-PUT-BUFFER                           
      *                        WS-COMPCODE                              
      *                        WS-REASON                                
      *    END-CALL                                                     
           DISPLAY 'SPECTER-MOCK:CALL:MQPUT1'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
           MOVE MOCK-NUM-STATUS TO RETURN-CODE
            IF WS-COMPCODE NOT = 0
              MOVE 'M004'                TO ERR-LOCATION                
              SET  ERR-CRITICAL          TO TRUE                        
              SET  ERR-MQ                TO TRUE                        
              MOVE WS-COMPCODE           TO WS-CODE-DISPLAY             
              MOVE WS-CODE-DISPLAY       TO ERR-CODE-1                  
              MOVE WS-REASON             TO WS-CODE-DISPLAY             
              MOVE WS-CODE-DISPLAY       TO ERR-CODE-2                  
              MOVE 'FAILED TO PUT ON REPLY MQ'                          
                                         TO ERR-MESSAGE                 
              MOVE PA-CARD-NUM           TO ERR-EVENT-KEY               
           DISPLAY 'SPECTER-CALL:FROM=7100-SEND-RESPONSE:TO=9500-LOG-E'
              PERFORM 9500-LOG-ERROR                                    
           END-IF                                                       
           .                                                            
```
