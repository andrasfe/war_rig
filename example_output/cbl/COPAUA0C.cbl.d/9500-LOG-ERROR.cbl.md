```cobol
       9500-LOG-ERROR.                                                          
      * ------------------------------------------------------------ *          
                                                                                
           EXEC CICS ASKTIME NOHANDLE                                           
              ABSTIME(WS-ABS-TIME)                                              
           END-EXEC                                                             
                                                                                
           EXEC CICS FORMATTIME                                                 
             ABSTIME(WS-ABS-TIME)                                               
             YYMMDD(WS-CUR-DATE-X6)                                             
             TIME(WS-CUR-TIME-X6)                                               
           END-EXEC                                                             
                                                                                
           MOVE WS-CICS-TRANID            TO ERR-APPLICATION                    
           MOVE WS-PGM-AUTH               TO ERR-PROGRAM                        
           MOVE WS-CUR-DATE-X6            TO ERR-DATE                           
           MOVE WS-CUR-TIME-X6            TO ERR-TIME                           
                                                                                
           EXEC CICS WRITEQ                                                     
                TD QUEUE('CSSL')                                                
                FROM (ERROR-LOG-RECORD)                                         
                LENGTH (LENGTH OF ERROR-LOG-RECORD)                             
                NOHANDLE                                                        
           END-EXEC                                                             
                                                                                
           IF ERR-CRITICAL                                                      
              PERFORM 9990-END-ROUTINE                                          
           END-IF                                                               
           .                                                                    
```
