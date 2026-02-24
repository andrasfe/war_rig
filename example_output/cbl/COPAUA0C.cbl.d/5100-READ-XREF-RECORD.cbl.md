```cobol
       5100-READ-XREF-RECORD.                                                   
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE PA-RQ-CARD-NUM           TO XREF-CARD-NUM                       
                                                                                
           EXEC CICS READ                                                       
                DATASET   (WS-CCXREF-FILE)                                      
                INTO      (CARD-XREF-RECORD)                                    
                LENGTH    (LENGTH OF CARD-XREF-RECORD)                          
                RIDFLD    (XREF-CARD-NUM)                                       
                KEYLENGTH (LENGTH OF XREF-CARD-NUM)                             
                RESP      (WS-RESP-CD)                                          
                RESP2     (WS-REAS-CD)                                          
           END-EXEC                                                             
                                                                                
           EVALUATE WS-RESP-CD                                                  
               WHEN DFHRESP(NORMAL)                                             
                   SET  CARD-FOUND-XREF  TO TRUE                                
               WHEN DFHRESP(NOTFND)                                             
                   SET  CARD-NFOUND-XREF TO TRUE                                
                   SET  NFOUND-ACCT-IN-MSTR TO TRUE                             
                                                                                
                   MOVE 'A001'          TO ERR-LOCATION                         
                   SET  ERR-WARNING     TO TRUE                                 
                   SET  ERR-APP         TO TRUE                                 
                   MOVE 'CARD NOT FOUND IN XREF'                                
                                        TO ERR-MESSAGE                          
                   MOVE XREF-CARD-NUM   TO ERR-EVENT-KEY                        
                   PERFORM 9500-LOG-ERROR                                       
               WHEN OTHER                                                       
                   MOVE 'C001'          TO ERR-LOCATION                         
                   SET  ERR-CRITICAL    TO TRUE                                 
                   SET  ERR-CICS        TO TRUE                                 
                   MOVE WS-RESP-CD      TO WS-CODE-DISPLAY                      
                   MOVE WS-CODE-DISPLAY TO ERR-CODE-1                           
                   MOVE WS-REAS-CD      TO WS-CODE-DISPLAY                      
                   MOVE WS-CODE-DISPLAY TO ERR-CODE-2                           
                   MOVE 'FAILED TO READ XREF FILE'                              
                                        TO ERR-MESSAGE                          
                   MOVE XREF-CARD-NUM   TO ERR-EVENT-KEY                        
                   PERFORM 9500-LOG-ERROR                                       
           END-EVALUATE                                                         
           .                                                                    
      *                                                                         
```
