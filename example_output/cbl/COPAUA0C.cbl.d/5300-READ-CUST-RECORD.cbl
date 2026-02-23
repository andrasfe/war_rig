       5300-READ-CUST-RECORD.                                                   
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE XREF-CUST-ID                 TO WS-CARD-RID-CUST-ID             
                                                                                
           EXEC CICS READ                                                       
                DATASET   (WS-CUSTFILENAME)                                     
                RIDFLD    (WS-CARD-RID-CUST-ID-X)                               
                KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)                     
                INTO      (CUSTOMER-RECORD)                                     
                LENGTH    (LENGTH OF CUSTOMER-RECORD)                           
                RESP      (WS-RESP-CD)                                          
                RESP2     (WS-REAS-CD)                                          
           END-EXEC                                                             
                                                                                
           EVALUATE WS-RESP-CD                                                  
               WHEN DFHRESP(NORMAL)                                             
                  SET FOUND-CUST-IN-MSTR     TO TRUE                            
               WHEN DFHRESP(NOTFND)                                             
                  SET NFOUND-CUST-IN-MSTR    TO TRUE                            
                                                                                
                  MOVE 'A003'                TO ERR-LOCATION                    
                  SET  ERR-WARNING           TO TRUE                            
                  SET  ERR-APP               TO TRUE                            
                  MOVE 'CUST NOT FOUND IN XREF'                                 
                                             TO ERR-MESSAGE                     
                  MOVE WS-CARD-RID-CUST-ID   TO ERR-EVENT-KEY                   
                  PERFORM 9500-LOG-ERROR                                        
      *                                                                         
               WHEN OTHER                                                       
                  MOVE 'C003'                TO ERR-LOCATION                    
                  SET  ERR-CRITICAL          TO TRUE                            
                  SET  ERR-CICS              TO TRUE                            
                  MOVE WS-RESP-CD            TO WS-CODE-DISPLAY                 
                  MOVE WS-CODE-DISPLAY       TO ERR-CODE-1                      
                  MOVE WS-REAS-CD            TO WS-CODE-DISPLAY                 
                  MOVE WS-CODE-DISPLAY       TO ERR-CODE-2                      
                  MOVE 'FAILED TO READ CUST FILE'                               
                                             TO ERR-MESSAGE                     
                  MOVE WS-CARD-RID-CUST-ID   TO ERR-EVENT-KEY                   
                  PERFORM 9500-LOG-ERROR                                        
           END-EVALUATE                                                         
           .                                                                    
      *                                                                         
       5300-EXIT.                                                               
           EXIT.                                                                
