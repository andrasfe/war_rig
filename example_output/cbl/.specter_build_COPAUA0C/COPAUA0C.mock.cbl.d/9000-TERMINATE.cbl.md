```cobol
       9000-TERMINATE.                                                  
           DISPLAY 'SPECTER-TRACE:9000-TERMINATE'.
      *
           IF IMS-PSB-SCHD                                              
      *       EXEC DLI TERM END-EXEC                                    
           DISPLAY 'SPECTER-MOCK:DLI-TERM'
           CONTINUE
           END-IF                                                       
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=9000-TERMINATE:TO=9100-CLOSE-REQ'
           PERFORM 9100-CLOSE-REQUEST-QUEUE THRU 9100-EXIT              
           .                                                            
```
