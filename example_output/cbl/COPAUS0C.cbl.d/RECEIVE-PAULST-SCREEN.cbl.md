```cobol
       RECEIVE-PAULST-SCREEN.
      *****************************************************************

           EXEC CICS RECEIVE
                     MAP('COPAU0A')
                     MAPSET('COPAU00')
                     INTO(COPAU0AI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC
           .
```
