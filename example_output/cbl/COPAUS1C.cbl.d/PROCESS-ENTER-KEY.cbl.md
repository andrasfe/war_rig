```cobol
       PROCESS-ENTER-KEY.

           MOVE LOW-VALUES          TO COPAU1AO
           IF CDEMO-ACCT-ID IS NUMERIC AND
              CDEMO-CPVD-PAU-SELECTED NOT = SPACES AND LOW-VALUES
              MOVE CDEMO-ACCT-ID            TO WS-ACCT-ID
              MOVE CDEMO-CPVD-PAU-SELECTED
                                            TO WS-AUTH-KEY
              PERFORM READ-AUTH-RECORD

              IF IMS-PSB-SCHD
                 SET IMS-PSB-NOT-SCHD      TO TRUE
                 PERFORM TAKE-SYNCPOINT
              END-IF

           ELSE
              SET ERR-FLG-ON                TO TRUE
           END-IF

           PERFORM POPULATE-AUTH-DETAILS
           .

```
