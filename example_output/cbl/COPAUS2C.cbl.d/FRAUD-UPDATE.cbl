       FRAUD-UPDATE.                                                            
           EXEC SQL                                                             
                UPDATE CARDDEMO.AUTHFRDS
                   SET   AUTH_FRAUD     = :AUTH-FRAUD,                          
                         FRAUD_RPT_DATE = CURRENT DATE                          
                   WHERE CARD_NUM = :CARD-NUM                                   
                     AND AUTH_TS  = TIMESTAMP_FORMAT (:AUTH-TS,                 
                                           'YY-MM-DD HH24.MI.SSNNNNNN')         
           END-EXEC                                                             
           IF SQLCODE = ZERO                                                    
              SET WS-FRD-UPDT-SUCCESS TO TRUE                                   
              MOVE 'UPDT SUCCESS'     TO WS-FRD-ACT-MSG                         
           ELSE                                                                 
              SET WS-FRD-UPDT-FAILED  TO TRUE                                   
                                                                                
              MOVE SQLCODE            TO WS-SQLCODE                             
              MOVE SQLSTATE           TO WS-SQLSTATE                            
                                                                                
              STRING ' UPDT ERROR DB2: CODE:' WS-SQLCODE                        
                     ', STATE: ' WS-SQLSTATE   DELIMITED BY SIZE                
              INTO WS-FRD-ACT-MSG                                               
              END-STRING                                                        
           END-IF                                                               
           .                                                                    
