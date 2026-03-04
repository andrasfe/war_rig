```cobol
         05 WS-DISPLAY-AMT9            PIC -zzzz9.99.                           
         05 WS-DISPLAY-COUNT           PIC 9(03).                               
         05 WS-AUTH-DATE               PIC X(08) VALUE '00/00/00'.              
         05 WS-AUTH-TIME               PIC X(08) VALUE '00:00:00'.              
                                                                                
      ******************************************************************        
      *      File and data Handling                                             
      ******************************************************************        
         05 WS-XREF-RID.                                                        
           10  WS-CARD-RID-CARDNUM                 PIC X(16).                   
           10  WS-CARD-RID-CUST-ID                 PIC 9(09).                   
           10  WS-CARD-RID-CUST-ID-X REDEFINES                                  
                  WS-CARD-RID-CUST-ID              PIC X(09).                   
           10  WS-CARD-RID-ACCT-ID                 PIC 9(11).                   
```
