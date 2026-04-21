```cobol
       2000-MAIN-PROCESS.                                               
           DISPLAY 'SPECTER-TRACE:2000-MAIN-PROCESS'.
      *
           PERFORM UNTIL NO-MORE-MSG-AVAILABLE OR WS-LOOP-END           
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=2000-MAIN-PROCESS:TO=2100-EXTRAC'
             PERFORM 2100-EXTRACT-REQUEST-MSG THRU 2100-EXIT            
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=2000-MAIN-PROCESS:TO=5000-PROCES'
             PERFORM 5000-PROCESS-AUTH        THRU 5000-EXIT            
                                                                        
             ADD 1                            TO WS-MSG-PROCESSED       
                                                                        
      *      EXEC CICS                                                  
      *           SYNCPOINT                                             
      *      END-EXEC                                                   
           DISPLAY 'SPECTER-CICS:SYNCPOINT'
           CONTINUE
             SET IMS-PSB-NOT-SCHD            TO TRUE                    
                                                                        
             IF WS-MSG-PROCESSED > WS-REQSTS-PROCESS-LIMIT              
                SET  WS-LOOP-END             TO TRUE                    
             ELSE                                                       
           DISPLAY 'SPECTER-CALL:FROM=2000-MAIN-PROCESS:TO=3100-READ-R'
                PERFORM 3100-READ-REQUEST-MQ THRU 3100-EXIT             
             END-IF                                                     
           END-PERFORM                                                  
           .                                                            
```
