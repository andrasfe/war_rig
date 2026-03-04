```cobol
       1000-INITIALIZE.                                                         
      * ------------------------------------------------------------- *         
      *                                                                         
           EXEC CICS RETRIEVE                                                   
             INTO(MQTM)                                                         
             NOHANDLE                                                           
           END-EXEC                                                             
           IF EIBRESP = DFHRESP(NORMAL)                                         
              MOVE MQTM-QNAME              TO WS-REQUEST-QNAME                  
              MOVE MQTM-TRIGGERDATA        TO WS-TRIGGER-DATA                   
           END-IF                                                               
                                                                                
           MOVE 5000                       TO WS-WAIT-INTERVAL                  
                                                                                
           PERFORM 1100-OPEN-REQUEST-QUEUE THRU 1100-EXIT                       
                                                                                
           PERFORM 3100-READ-REQUEST-MQ    THRU 3100-EXIT                       
           .                                                                    
      *                                                                         
```
