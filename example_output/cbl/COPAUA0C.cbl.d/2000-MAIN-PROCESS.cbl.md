```cobol
       2000-MAIN-PROCESS.                                                       
      * ------------------------------------------------------------- *         
      *                                                                         
           PERFORM UNTIL NO-MORE-MSG-AVAILABLE OR WS-LOOP-END                   
                                                                                
             PERFORM 2100-EXTRACT-REQUEST-MSG THRU 2100-EXIT                    
                                                                                
             PERFORM 5000-PROCESS-AUTH        THRU 5000-EXIT                    
                                                                                
             ADD 1                            TO WS-MSG-PROCESSED               
                                                                                
             EXEC CICS                                                          
                  SYNCPOINT                                                     
             END-EXEC                                                           
             SET IMS-PSB-NOT-SCHD            TO TRUE                            
                                                                                
             IF WS-MSG-PROCESSED > WS-REQSTS-PROCESS-LIMIT                      
                SET  WS-LOOP-END             TO TRUE                            
             ELSE                                                               
                PERFORM 3100-READ-REQUEST-MQ THRU 3100-EXIT                     
             END-IF                                                             
           END-PERFORM                                                          
           .                                                                    
      *                                                                         
```
