```cobol
       9500-LOG-ERROR.                                                  
           DISPLAY 'SPECTER-TRACE:9500-LOG-ERROR'.
      *
                                                                        
      *    EXEC CICS ASKTIME NOHANDLE                                   
      *       ABSTIME(WS-ABS-TIME)                                      
      *    END-EXEC                                                     
           DISPLAY 'SPECTER-MOCK:CICS'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '00' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
                                                                        
      *    EXEC CICS FORMATTIME                                         
      *      ABSTIME(WS-ABS-TIME)                                       
      *      YYMMDD(WS-CUR-DATE-X6)                                     
      *      TIME(WS-CUR-TIME-X6)                                       
      *    END-EXEC                                                     
           DISPLAY 'SPECTER-MOCK:CICS'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '00' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
                                                                        
           MOVE WS-CICS-TRANID            TO ERR-APPLICATION            
           MOVE WS-PGM-AUTH               TO ERR-PROGRAM                
           MOVE WS-CUR-DATE-X6            TO ERR-DATE                   
           MOVE WS-CUR-TIME-X6            TO ERR-TIME                   
                                                                        
      *    EXEC CICS WRITEQ                                             
      *         TD QUEUE('CSSL')                                        
      *         FROM (ERROR-LOG-RECORD)                                 
      *         LENGTH (LENGTH OF ERROR-LOG-RECORD)                     
      *         NOHANDLE                                                
      *    END-EXEC                                                     
           DISPLAY 'SPECTER-MOCK:CICS-WRITEQ'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '00' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
                                                                        
           IF ERR-CRITICAL                                              
           DISPLAY 'SPECTER-CALL:FROM=9500-LOG-ERROR:TO=9990-END-ROUTI'
              PERFORM 9990-END-ROUTINE                                  
           END-IF                                                       
           .                                                            
```
