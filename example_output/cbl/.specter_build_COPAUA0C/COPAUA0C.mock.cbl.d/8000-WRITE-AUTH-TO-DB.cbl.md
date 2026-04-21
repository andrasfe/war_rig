```cobol
       8000-WRITE-AUTH-TO-DB.                                           
           DISPLAY 'SPECTER-TRACE:8000-WRITE-AUTH-TO-DB'.
      *
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=8000-WRITE-AUTH-TO-DB:TO=8400-UP'
           PERFORM 8400-UPDATE-SUMMARY      THRU 8400-EXIT              
           DISPLAY 'SPECTER-CALL:FROM=8000-WRITE-AUTH-TO-DB:TO=8500-IN'
           PERFORM 8500-INSERT-AUTH         THRU 8500-EXIT              
           .                                                            
```
