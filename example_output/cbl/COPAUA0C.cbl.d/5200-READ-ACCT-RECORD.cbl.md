```cobol
       5200-READ-ACCT-RECORD.                                                   
      * ------------------------------------------------------------- *         
      *                                                                         
           MOVE XREF-ACCT-ID          TO WS-CARD-RID-ACCT-ID                    
                                                                                
           EXEC CICS READ                                                       
                DATASET   (WS-ACCTFILENAME)                                     
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)                               
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)                     
                INTO      (ACCOUNT-RECORD)                                      
                LENGTH    (LENGTH OF ACCOUNT-RECORD)                            
                RESP      (WS-RESP-CD)                                          
                RESP2     (WS-REAS-CD)                                          
           END-EXEC                                                             
                                                                                
           EVALUATE WS-RESP-CD                                                  
               WHEN DFHRESP(NORMAL)                                             
                  SET FOUND-ACCT-IN-MSTR     TO TRUE                            
               WHEN DFHRESP(NOTFND)                                             
                  SET NFOUND-ACCT-IN-MSTR    TO TRUE                            
                                                                                
                  MOVE 'A002'                TO ERR-LOCATION                    
                  SET  ERR-WARNING           TO TRUE                            
                  SET  ERR-APP               TO TRUE                            
                  MOVE 'ACCT NOT FOUND IN XREF'                                 
                                             TO ERR-MESSAGE                     
```
