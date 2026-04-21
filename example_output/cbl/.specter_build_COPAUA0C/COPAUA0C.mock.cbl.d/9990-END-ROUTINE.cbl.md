```cobol
       9990-END-ROUTINE.                                                
           DISPLAY 'SPECTER-TRACE:9990-END-ROUTINE'.
      *
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=9990-END-ROUTINE:TO=9000-TERMINA'
           PERFORM 9000-TERMINATE                                       
                                                                        
      *    EXEC CICS RETURN                                             
      *    END-EXEC                                                     
           DISPLAY 'SPECTER-MOCK:CICS-RETURN'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '00' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
           CONTINUE
           .                                                            
```
