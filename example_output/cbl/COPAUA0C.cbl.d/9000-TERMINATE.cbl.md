```cobol
       9000-TERMINATE.                                                          
      * ------------------------------------------------------------- *         
      *                                                                         
           IF IMS-PSB-SCHD                                                      
              EXEC DLI TERM END-EXEC                                            
           END-IF                                                               
                                                                                
           PERFORM 9100-CLOSE-REQUEST-QUEUE THRU 9100-EXIT                      
           .                                                                    
      *                                                                         
       9000-EXIT.                                                               
```
