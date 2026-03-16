```cobol
       SEND-AUTHVIEW-SCREEN.

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COPAU1AO
           MOVE -1       TO CARDNUML

           IF SEND-ERASE-YES
              EXEC CICS SEND
                     MAP('COPAU1A')
                     MAPSET('COPAU01')
                     FROM(COPAU1AO)
                     ERASE
                     CURSOR
              END-EXEC
           ELSE
              EXEC CICS SEND
                     MAP('COPAU1A')
                     MAPSET('COPAU01')
                     FROM(COPAU1AO)
                     CURSOR
              END-EXEC
           END-IF
           .
```
