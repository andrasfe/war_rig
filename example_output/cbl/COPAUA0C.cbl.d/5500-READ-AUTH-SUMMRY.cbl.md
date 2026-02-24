```cobol
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.             
                                                                                
         05 WS-ABS-TIME                PIC S9(15) COMP-3 VALUE 0.               
         05 WS-CUR-DATE-X6             PIC X(06)  VALUE SPACES.                 
         05 WS-CUR-TIME-X6             PIC X(06)  VALUE SPACES.                 
         05 WS-CUR-TIME-N6             PIC 9(06)  VALUE ZERO.                   
         05 WS-CUR-TIME-MS             PIC S9(08) COMP.                         
         05 WS-YYDDD                   PIC 9(05).                               
         05 WS-TIME-WITH-MS            PIC S9(09) COMP-3.                       
         05 WS-OPTIONS                 PIC S9(9)  BINARY.                       
         05 WS-COMPCODE                PIC S9(9)  BINARY.                       
         05 WS-REASON                  PIC S9(9)  BINARY.                       
         05 WS-WAIT-INTERVAL           PIC S9(9)  BINARY.                       
         05 WS-CODE-DISPLAY            PIC 9(9).                                
         05 WS-AVAILABLE-AMT           PIC S9(09)V99 COMP-3.                    
         05 WS-TRANSACTION-AMT-AN      PIC X(13).                               
         05 WS-TRANSACTION-AMT         PIC S9(10)V99.                           
         05 WS-APPROVED-AMT            PIC S9(10)V99.                           
         05 WS-APPROVED-AMT-DIS        PIC -zzzzzzzzz9.99.                      
         05 WS-TRIGGER-DATA            PIC X(64).                               
                                                                                
      ******************************************************************        
      *      File and data Handling                                             
      ******************************************************************        
         05 WS-XREF-RID.                                                        
           10  WS-CARD-RID-CARDNUM                 PIC X(16).                   
```
