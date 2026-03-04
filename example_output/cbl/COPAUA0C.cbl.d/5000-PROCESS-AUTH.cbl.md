```cobol
       5000-PROCESS-AUTH.                                                       
      * ------------------------------------------------------------- *         
      *                                                                         
           SET APPROVE-AUTH                  TO TRUE                            
                                                                                
           PERFORM 1200-SCHEDULE-PSB         THRU 1200-EXIT                     
                                                                                
           SET CARD-FOUND-XREF               TO TRUE                            
           SET FOUND-ACCT-IN-MSTR            TO TRUE                            
                                                                                
           PERFORM 5100-READ-XREF-RECORD     THRU 5100-EXIT                     
                                                                                
           IF CARD-FOUND-XREF                                                   
              PERFORM 5200-READ-ACCT-RECORD  THRU 5200-EXIT                     
              PERFORM 5300-READ-CUST-RECORD  THRU 5300-EXIT                     
                                                                                
              PERFORM 5500-READ-AUTH-SUMMRY  THRU 5500-EXIT                     
                                                                                
              PERFORM 5600-READ-PROFILE-DATA THRU 5600-EXIT                     
           END-IF                                                               
                                                                                
           PERFORM 6000-MAKE-DECISION        THRU 6000-EXIT                     
                                                                                
           PERFORM 7100-SEND-RESPONSE        THRU 7100-EXIT                     
                                                                                
           IF CARD-FOUND-XREF                                                   
              PERFORM 8000-WRITE-AUTH-TO-DB  THRU 8000-EXIT                     
           END-IF                                                               
           .                                                                    
      *                                                                         
```
