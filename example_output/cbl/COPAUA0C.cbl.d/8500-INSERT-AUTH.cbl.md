```cobol
       8500-INSERT-AUTH.                                                        
      * ------------------------------------------------------------- *         
      *                                                                         
           EXEC CICS ASKTIME NOHANDLE                                           
              ABSTIME(WS-ABS-TIME)                                              
           END-EXEC                                                             
                                                                                
           EXEC CICS FORMATTIME                                                 
             ABSTIME(WS-ABS-TIME)                                               
             YYDDD(WS-CUR-DATE-X6)                                              
             TIME(WS-CUR-TIME-X6)                                               
             MILLISECONDS(WS-CUR-TIME-MS)                                       
           END-EXEC                                                             
                                                                                
           MOVE WS-CUR-DATE-X6(1:5)         TO WS-YYDDD                         
           MOVE WS-CUR-TIME-X6              TO WS-CUR-TIME-N6                   
                                                                                
           COMPUTE WS-TIME-WITH-MS = (WS-CUR-TIME-N6 * 1000) +                  
                                     WS-CUR-TIME-MS                             
                                                                                
           COMPUTE PA-AUTH-DATE-9C = 99999 - WS-YYDDD                           
           COMPUTE PA-AUTH-TIME-9C = 999999999 - WS-TIME-WITH-MS                
                                                                                
           MOVE PA-RQ-AUTH-DATE             TO PA-AUTH-ORIG-DATE                
           MOVE PA-RQ-AUTH-TIME             TO PA-AUTH-ORIG-TIME                
           MOVE PA-RQ-CARD-NUM              TO PA-CARD-NUM                      
           MOVE PA-RQ-AUTH-TYPE             TO PA-AUTH-TYPE                     
           MOVE PA-RQ-CARD-EXPIRY-DATE      TO PA-CARD-EXPIRY-DATE              
           MOVE PA-RQ-MESSAGE-TYPE          TO PA-MESSAGE-TYPE                  
           MOVE PA-RQ-MESSAGE-SOURCE        TO PA-MESSAGE-SOURCE                
           MOVE PA-RQ-PROCESSING-CODE       TO PA-PROCESSING-CODE               
           MOVE PA-RQ-TRANSACTION-AMT       TO PA-TRANSACTION-AMT               
           MOVE PA-RQ-MERCHANT-CATAGORY-CODE                                    
                                            TO PA-MERCHANT-CATAGORY-CODE        
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
             PERFORM 9500-LOG-ERROR                                             
           END-IF                                                               
           .                                                                    
      *                                                                         
```
