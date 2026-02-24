```cobol
       2100-EXTRACT-REQUEST-MSG.                                                
      * ------------------------------------------------------------- *         
      *                                                                         
           UNSTRING W01-GET-BUFFER(1:W01-DATALEN)                               
                    DELIMITED BY ','                                            
                    INTO PA-RQ-AUTH-DATE                                        
                         PA-RQ-AUTH-TIME                                        
                         PA-RQ-CARD-NUM                                         
                         PA-RQ-AUTH-TYPE                                        
                         PA-RQ-CARD-EXPIRY-DATE                                 
                         PA-RQ-MESSAGE-TYPE                                     
                         PA-RQ-MESSAGE-SOURCE                                   
                         PA-RQ-PROCESSING-CODE                                  
                         WS-TRANSACTION-AMT-AN                                  
                         PA-RQ-MERCHANT-CATAGORY-CODE                           
                         PA-RQ-ACQR-COUNTRY-CODE                                
                         PA-RQ-POS-ENTRY-MODE                                   
                         PA-RQ-MERCHANT-ID                                      
                         PA-RQ-MERCHANT-NAME                                    
                         PA-RQ-MERCHANT-CITY                                    
                         PA-RQ-MERCHANT-STATE                                   
                         PA-RQ-MERCHANT-ZIP                                     
                         PA-RQ-TRANSACTION-ID                                   
           END-UNSTRING                                                         
                                                                                
           COMPUTE PA-RQ-TRANSACTION-AMT =                                      
                               FUNCTION NUMVAL(WS-TRANSACTION-AMT-AN)           
                                                                                
           MOVE PA-RQ-TRANSACTION-AMT  TO WS-TRANSACTION-AMT                    
           .                                                                    
      *                                                                         
       2100-EXIT.                                                               
           EXIT.                                                                
      *                                                                         
      * ------------------------------------------------------------- *         
```
