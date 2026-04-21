```cobol
       1200-SCHEDULE-PSB.                                               
           DISPLAY 'SPECTER-TRACE:1200-SCHEDULE-PSB'.
      *    EXEC DLI SCHD                                                
      *         PSB((PSB-NAME))                                         
      *         NODHABEND                                               
      *    END-EXEC                                                     
           DISPLAY 'SPECTER-MOCK:DLI-SCHD'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '  ' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
           MOVE MOCK-ALPHA-STATUS TO DIBSTAT
           MOVE DIBSTAT        TO IMS-RETURN-CODE                       
           IF PSB-SCHEDULED-MORE-THAN-ONCE                              
      *       EXEC DLI TERM                                             
      *       END-EXEC                                                  
           DISPLAY 'SPECTER-MOCK:DLI-TERM'
           CONTINUE
                                                                        
      *       EXEC DLI SCHD                                             
      *            PSB((PSB-NAME))                                      
      *            NODHABEND                                            
      *       END-EXEC                                                  
           DISPLAY 'SPECTER-MOCK:DLI-SCHD'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '  ' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
           MOVE MOCK-ALPHA-STATUS TO DIBSTAT
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
           DISPLAY 'SPECTER-CALL:FROM=1200-SCHEDULE-PSB:TO=9500-LOG-ER'
              PERFORM 9500-LOG-ERROR                                    
           END-IF                                                       
           .
```
