```cobol
       6000-MAKE-DECISION.                                                      
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE PA-RQ-CARD-NUM         TO PA-RL-CARD-NUM                        
           MOVE PA-RQ-TRANSACTION-ID   TO PA-RL-TRANSACTION-ID                  
           MOVE PA-RQ-AUTH-TIME        TO PA-RL-AUTH-ID-CODE                    
                                                                                
      *-   Decline Auth if Above Limit; If no AUTH summary, use ACT data        
           IF FOUND-PAUT-SMRY-SEG                                               
              COMPUTE WS-AVAILABLE-AMT = PA-CREDIT-LIMIT                        
                                       - PA-CREDIT-BALANCE                      
              IF WS-TRANSACTION-AMT > WS-AVAILABLE-AMT                          
                 SET DECLINE-AUTH      TO TRUE                                  
                 SET INSUFFICIENT-FUND TO TRUE                                  
              END-IF                                                            
           ELSE                                                                 
              IF FOUND-ACCT-IN-MSTR                                             
                 COMPUTE WS-AVAILABLE-AMT = ACCT-CREDIT-LIMIT                   
                                          - ACCT-CURR-BAL                       
                 IF WS-TRANSACTION-AMT > WS-AVAILABLE-AMT                       
                    SET DECLINE-AUTH      TO TRUE                               
                    SET INSUFFICIENT-FUND TO TRUE                               
                 END-IF                                                         
              ELSE                                                              
                 SET DECLINE-AUTH         TO TRUE                               
              END-IF                                                            
           END-IF                                                               
                                                                                
           IF DECLINE-AUTH                                                      
              SET  AUTH-RESP-DECLINED     TO TRUE                               
                                                                                
              MOVE '05'                   TO PA-RL-AUTH-RESP-CODE               
              MOVE 0                      TO PA-RL-APPROVED-AMT                 
                                             WS-APPROVED-AMT                    
           ELSE                                                                 
              SET  AUTH-RESP-APPROVED     TO TRUE                               
              MOVE '00'                   TO PA-RL-AUTH-RESP-CODE               
              MOVE PA-RQ-TRANSACTION-AMT  TO PA-RL-APPROVED-AMT                 
                                             WS-APPROVED-AMT                    
           END-IF                                                               
                                                                                
           MOVE '0000'                    TO PA-RL-AUTH-RESP-REASON             
           IF AUTH-RESP-DECLINED                                                
              EVALUATE TRUE                                                     
                 WHEN CARD-NFOUND-XREF                                          
                 WHEN NFOUND-ACCT-IN-MSTR                                       
                 WHEN NFOUND-CUST-IN-MSTR                                       
                      MOVE '3100'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN INSUFFICIENT-FUND                                         
                      MOVE '4100'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN CARD-NOT-ACTIVE                                           
                      MOVE '4200'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN ACCOUNT-CLOSED                                            
                      MOVE '4300'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN CARD-FRAUD                                                
                      MOVE '5100'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN MERCHANT-FRAUD                                            
                      MOVE '5200'         TO PA-RL-AUTH-RESP-REASON             
                 WHEN OTHER                                                     
                      MOVE '9000'         TO PA-RL-AUTH-RESP-REASON             
              END-EVALUATE                                                      
           END-IF                                                               
                                                                                
           MOVE WS-APPROVED-AMT        TO WS-APPROVED-AMT-DIS                   
                                                                                
           STRING PA-RL-CARD-NUM         ','                                    
                  PA-RL-TRANSACTION-ID   ','                                    
                  PA-RL-AUTH-ID-CODE     ','                                    
                  PA-RL-AUTH-RESP-CODE   ','                                    
                  PA-RL-AUTH-RESP-REASON ','                                    
                  WS-APPROVED-AMT-DIS    ','                                    
                  DELIMITED BY SIZE                                             
                  INTO W02-PUT-BUFFER                                           
                  WITH POINTER WS-RESP-LENGTH                                   
           END-STRING                                                           
           .                                                                    
      *                                                                         
```
