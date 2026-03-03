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
```
