```cobol
       5500-READ-AUTH-SUMMRY.                                           
           DISPLAY 'SPECTER-TRACE:5500-READ-AUTH-SUMMRY'.
      *
             MOVE WS-CARD-RID-ACCT-ID-X    TO PA-ACCT-ID
      *    EXEC DLI GU USING PCB(PAUT-PCB-NUM)                          
      *        SEGMENT (PAUTSUM0)                                       
      *        INTO (PENDING-AUTH-SUMMARY)                              
      *        WHERE (ACCNTID = PA-ACCT-ID)                             
      *    END-EXEC                                                     
           DISPLAY 'SPECTER-MOCK:DLI-GU'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '  ' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
           MOVE MOCK-ALPHA-STATUS TO DIBSTAT
                                                                        
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
           DISPLAY 'SPECTER-CALL:FROM=5500-READ-AUTH-SUMMRY:TO=9500-LO'
                  PERFORM 9500-LOG-ERROR                                
           END-EVALUATE                                                 
           .                                                            
```
