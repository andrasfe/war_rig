```cobol
       5500-READ-AUTH-SUMMRY.                                                   
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE XREF-ACCT-ID                    TO PA-ACCT-ID                   
           EXEC DLI GU USING PCB(PAUT-PCB-NUM)                                  
               SEGMENT (PAUTSUM0)                                               
               INTO (PENDING-AUTH-SUMMARY)                                      
               WHERE (ACCNTID = PA-ACCT-ID)                                     
           END-EXEC                                                             
                                                                                
           MOVE DIBSTAT                          TO IMS-RETURN-CODE             
           EVALUATE TRUE                                                        
               WHEN STATUS-OK                                                   
                  SET FOUND-PAUT-SMRY-SEG        TO TRUE                        
               WHEN SEGMENT-NOT-FOUND                                           
                  SET NFOUND-PAUT-SMRY-SEG       TO TRUE                        
               WHEN OTHER                                                       
                  MOVE 'I002'                    TO ERR-LOCATION                
                  SET  ERR-CRITICAL              TO TRUE                        
                  SET  ERR-IMS                   TO TRUE                        
                  MOVE IMS-RETURN-CODE           TO ERR-CODE-1                  
                  MOVE 'IMS GET SUMMARY FAILED'  TO ERR-MESSAGE                 
                  MOVE PA-CARD-NUM               TO ERR-EVENT-KEY               
                  PERFORM 9500-LOG-ERROR                                        
           END-EVALUATE                                                         
           .                                                                    
      *                                                                         
       5500-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
       5600-READ-PROFILE-DATA.                                                  
      * ------------------------------------------------------------- *         
      *                                                                         
           CONTINUE                                                             
           .                                                                    
      *                                                                         
       5600-EXIT.                                                               
           EXIT.                                                                
```
