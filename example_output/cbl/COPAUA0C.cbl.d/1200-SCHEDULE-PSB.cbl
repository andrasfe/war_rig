       1200-SCHEDULE-PSB.                                               08490000
           EXEC DLI SCHD                                                        
                PSB((PSB-NAME))                                                 
                NODHABEND                                                       
           END-EXEC                                                             
           MOVE DIBSTAT        TO IMS-RETURN-CODE                               
           IF PSB-SCHEDULED-MORE-THAN-ONCE                                      
              EXEC DLI TERM                                                     
              END-EXEC                                                          
                                                                                
              EXEC DLI SCHD                                                     
                   PSB((PSB-NAME))                                              
                   NODHABEND                                                    
              END-EXEC                                                          
              MOVE DIBSTAT     TO IMS-RETURN-CODE                               
           END-IF                                                               
           IF STATUS-OK                                                         
              SET IMS-PSB-SCHD           TO TRUE                                
           ELSE                                                                 
              MOVE 'I001'                TO ERR-LOCATION                        
              SET  ERR-CRITICAL          TO TRUE                                
              SET  ERR-IMS               TO TRUE                                
              MOVE IMS-RETURN-CODE       TO ERR-CODE-1                          
              MOVE 'IMS SCHD FAILED'     TO ERR-MESSAGE                         
              PERFORM 9500-LOG-ERROR                                            
           END-IF                                                               
           .
