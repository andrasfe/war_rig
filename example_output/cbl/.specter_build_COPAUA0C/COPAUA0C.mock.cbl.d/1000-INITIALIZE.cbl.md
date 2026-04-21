```cobol
       1000-INITIALIZE.                                                 
           DISPLAY 'SPECTER-TRACE:1000-INITIALIZE'.
      *
      *    EXEC CICS RETRIEVE                                           
      *      INTO(MQTM)                                                 
      *      NOHANDLE                                                   
      *    END-EXEC                                                     
           DISPLAY 'SPECTER-MOCK:CICS'
           READ MOCK-FILE INTO MOCK-RECORD
              AT END
                MOVE '00' TO MOCK-ALPHA-STATUS
                MOVE 0 TO MOCK-NUM-STATUS
           END-READ
            IF WS-RESP-CD = 0
               MOVE WS-REQUEST-QNAME       TO WS-REQUEST-QNAME
               MOVE SPACES                 TO WS-TRIGGER-DATA
           END-IF                                                       
                                                                        
           MOVE 5000                       TO WS-WAIT-INTERVAL          
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=1000-INITIALIZE:TO=1100-OPEN-REQ'
           PERFORM 1100-OPEN-REQUEST-QUEUE THRU 1100-EXIT               
                                                                        
           DISPLAY 'SPECTER-CALL:FROM=1000-INITIALIZE:TO=3100-READ-REQ'
           PERFORM 3100-READ-REQUEST-MQ    THRU 3100-EXIT               
           .                                                            
```
