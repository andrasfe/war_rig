```cobol
       SEND-PAULST-SCREEN.
      *****************************************************************

           IF IMS-PSB-SCHD
              SET IMS-PSB-NOT-SCHD      TO TRUE
              EXEC CICS SYNCPOINT
              END-EXEC
           END-IF

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COPAU0AO

           IF SEND-ERASE-YES
               EXEC CICS SEND
                         MAP('COPAU0A')
                         MAPSET('COPAU00')
                         FROM(COPAU0AO)
                         ERASE
                         CURSOR
               END-EXEC
           ELSE
               EXEC CICS SEND
                         MAP('COPAU0A')
                         MAPSET('COPAU00')
                         FROM(COPAU0AO)
                         CURSOR
               END-EXEC
           END-IF.

```
