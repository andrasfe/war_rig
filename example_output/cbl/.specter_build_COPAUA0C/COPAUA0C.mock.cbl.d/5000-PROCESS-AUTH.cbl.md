```cobol
       5000-PROCESS-AUTH.                                               
           DISPLAY 'SPECTER-TRACE:5000-PROCESS-AUTH'.
      *
           SET APPROVE-AUTH                  TO TRUE                    
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=5000-PROCESS-AUTH:TO=1200-SCHEDU'
           PERFORM 1200-SCHEDULE-PSB         THRU 1200-EXIT             
                                                                        
           SET CARD-FOUND-XREF               TO TRUE                    
           SET FOUND-ACCT-IN-MSTR            TO TRUE                    
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=5000-PROCESS-AUTH:TO=5100-READ-X'
           PERFORM 5100-READ-XREF-RECORD     THRU 5100-EXIT             
                                                                        
           IF CARD-FOUND-XREF                                           
           DISPLAY 'SPECTER-CALL:FROM=5000-PROCESS-AUTH:TO=5200-READ-A'
              PERFORM 5200-READ-ACCT-RECORD  THRU 5200-EXIT             
           DISPLAY 'SPECTER-CALL:FROM=5000-PROCESS-AUTH:TO=5300-READ-C'
              PERFORM 5300-READ-CUST-RECORD  THRU 5300-EXIT             
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=5000-PROCESS-AUTH:TO=5500-READ-A'
              PERFORM 5500-READ-AUTH-SUMMRY  THRU 5500-EXIT             
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=5000-PROCESS-AUTH:TO=5600-READ-P'
              PERFORM 5600-READ-PROFILE-DATA THRU 5600-EXIT             
           END-IF                                                       
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=5000-PROCESS-AUTH:TO=6000-MAKE-D'
           PERFORM 6000-MAKE-DECISION        THRU 6000-EXIT             
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=5000-PROCESS-AUTH:TO=7100-SEND-R'
           PERFORM 7100-SEND-RESPONSE        THRU 7100-EXIT             
                                                                        
           IF CARD-FOUND-XREF                                           
           DISPLAY 'SPECTER-CALL:FROM=5000-PROCESS-AUTH:TO=8000-WRITE-'
              PERFORM 8000-WRITE-AUTH-TO-DB  THRU 8000-EXIT             
           END-IF                                                       
           .                                                            
```
