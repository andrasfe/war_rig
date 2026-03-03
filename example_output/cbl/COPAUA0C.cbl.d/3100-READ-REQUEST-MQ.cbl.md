```cobol
           CALL 'MQGET' USING W01-HCONN-REQUEST                                 
                              W01-HOBJ-REQUEST                                  
                              MQM-MD-REQUEST                                    
                              MQM-GET-MESSAGE-OPTIONS                           
                              W01-BUFFLEN                                       
                              W01-GET-BUFFER                                    
                              W01-DATALEN                                       
                              WS-COMPCODE                                       
                              WS-REASON                                         
           END-CALL                                                             
           IF WS-COMPCODE = MQCC-OK                                             
              MOVE MQMD-CORRELID OF MQM-MD-REQUEST                              
                                           TO WS-SAVE-CORRELID                  
              MOVE MQMD-REPLYTOQ OF MQM-MD-REQUEST                              
                                           TO WS-REPLY-QNAME                    
           ELSE                                                                 
              IF WS-REASON = MQRC-NO-MSG-AVAILABLE                              
                 SET NO-MORE-MSG-AVAILABLE TO TRUE                              
              ELSE                                                              
                MOVE 'M003'                TO ERR-LOCATION                      
                SET  ERR-CRITICAL          TO TRUE                              
                SET  ERR-CICS              TO TRUE                              
                MOVE WS-COMPCODE           TO WS-CODE-DISPLAY                   
                MOVE WS-CODE-DISPLAY       TO ERR-CODE-1                        
                MOVE WS-REASON             TO WS-CODE-DISPLAY                   
                MOVE WS-CODE-DISPLAY       TO ERR-CODE-2                        
                MOVE 'FAILED TO READ REQUEST MQ'                                
                                           TO ERR-MESSAGE                       
                MOVE PA-CARD-NUM           TO ERR-EVENT-KEY                     
                PERFORM 9500-LOG-ERROR                                          
              END-IF                                                            
           END-IF                                                               
```
