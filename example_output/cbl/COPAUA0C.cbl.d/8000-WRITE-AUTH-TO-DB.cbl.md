```cobol
       8000-WRITE-AUTH-TO-DB.                                                   
      * ------------------------------------------------------------- *         
      *                                                                         
                                                                                
           PERFORM 8400-UPDATE-SUMMARY      THRU 8400-EXIT                      
           PERFORM 8500-INSERT-AUTH         THRU 8500-EXIT                      
           .                                                                    
      *                                                                         
       8000-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
```
