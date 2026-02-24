```cobol
           MOVE PA-RQ-ACQR-COUNTRY-CODE     TO PA-ACQR-COUNTRY-CODE             
           MOVE PA-RQ-POS-ENTRY-MODE        TO PA-POS-ENTRY-MODE                
           MOVE PA-RQ-MERCHANT-ID           TO PA-MERCHANT-ID                   
           MOVE PA-RQ-MERCHANT-NAME         TO PA-MERCHANT-NAME                 
           MOVE PA-RQ-MERCHANT-CITY         TO PA-MERCHANT-CITY                 
           MOVE PA-RQ-MERCHANT-STATE        TO PA-MERCHANT-STATE                
           MOVE PA-RQ-MERCHANT-ZIP          TO PA-MERCHANT-ZIP                  
           MOVE PA-RQ-TRANSACTION-ID        TO PA-TRANSACTION-ID                
                                                                                
           MOVE PA-RL-AUTH-ID-CODE          TO PA-AUTH-ID-CODE                  
           MOVE PA-RL-AUTH-RESP-CODE        TO PA-AUTH-RESP-CODE                
           MOVE PA-RL-AUTH-RESP-REASON      TO PA-AUTH-RESP-REASON              
           MOVE PA-RL-APPROVED-AMT          TO PA-APPROVED-AMT                  
                                                                                
           IF AUTH-RESP-APPROVED                                                
              SET  PA-MATCH-PENDING         TO TRUE
           ELSE                                                                 
              SET  PA-MATCH-AUTH-DECLINED   TO TRUE
           END-IF                                                               

           MOVE SPACE                       TO PA-AUTH-FRAUD
                                               PA-FRAUD-RPT-DATE

           MOVE XREF-ACCT-ID                TO PA-ACCT-ID                       
                                                                                
           EXEC DLI ISRT USING PCB(PAUT-PCB-NUM)
                SEGMENT (PAUTSUM0)
                WHERE (ACCNTID = PA-ACCT-ID)
                SEGMENT (PAUTDTL1)
                FROM (PENDING-AUTH-DETAILS)
                SEGLENGTH (LENGTH OF PENDING-AUTH-DETAILS)
           END-EXEC
           MOVE DIBSTAT                     TO IMS-RETURN-CODE                  
                                                                                
           IF STATUS-OK                                                         
             CONTINUE                                                           
           ELSE                                                                 
             MOVE 'I004'                    TO ERR-LOCATION                     
             SET  ERR-CRITICAL              TO TRUE                             
             SET  ERR-IMS                   TO TRUE                             
             MOVE IMS-RETURN-CODE           TO ERR-CODE-1                       
             MOVE 'IMS INSERT DETL FAILED'  TO ERR-MESSAGE                      
             MOVE PA-CARD-NUM               TO ERR-EVENT-KEY                    
```
