```cobol
       MAIN-PARA.                                                       
           OPEN INPUT MOCK-FILE
           DISPLAY 'SPECTER-TRACE:MAIN-PARA'.
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=MAIN-PARA:TO=1000-INITIALIZE'
           PERFORM 1000-INITIALIZE    THRU 1000-EXIT                    
           DISPLAY 'SPECTER-CALL:FROM=MAIN-PARA:TO=2000-MAIN-PROCESS'
           PERFORM 2000-MAIN-PROCESS  THRU 2000-EXIT                    
           DISPLAY 'SPECTER-CALL:FROM=MAIN-PARA:TO=9000-TERMINATE'
           PERFORM 9000-TERMINATE     THRU 9000-EXIT                    
                                                                        
      *    EXEC CICS RETURN                                             
      *    END-EXEC.                                                    
           DISPLAY 'SPECTER-MOCK:CICS-RETURN'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '00' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
           CONTINUE.
```
