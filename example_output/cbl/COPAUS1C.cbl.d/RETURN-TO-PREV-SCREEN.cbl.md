```cobol
           .

       PROCESS-ENTER-KEY.

           MOVE LOW-VALUES          TO COPAU1AO
           IF CDEMO-ACCT-ID IS NUMERIC AND
              CDEMO-CPVD-PAU-SELECTED NOT = SPACES AND LOW-VALUES
              MOVE CDEMO-ACCT-ID            TO WS-ACCT-ID
              MOVE CDEMO-CPVD-PAU-SELECTED
                                            TO WS-AUTH-KEY
              PERFORM READ-AUTH-RECORD
```
