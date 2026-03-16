```cobol
       RECEIVE-AUTHVIEW-SCREEN.

           EXEC CICS RECEIVE
                     MAP('COPAU1A')
                     MAPSET('COPAU01')
                     INTO(COPAU1AI)
                     NOHANDLE
           END-EXEC
           .
```
