```cobol

                  IF CDEMO-ACCT-ID IS NUMERIC
                     MOVE CDEMO-ACCT-ID     TO WS-ACCT-ID
                                               ACCTIDO OF COPAU0AO
                  ELSE
                     MOVE SPACE             TO ACCTIDO OF COPAU0AO
                     MOVE LOW-VALUES        TO WS-ACCT-ID
                  END-IF

                  PERFORM GATHER-DETAILS

                  SET SEND-ERASE-YES TO TRUE

                  PERFORM SEND-PAULST-SCREEN

               ELSE
                  PERFORM RECEIVE-PAULST-SCREEN

                  EVALUATE EIBAID
                     WHEN DFHENTER
                       PERFORM PROCESS-ENTER-KEY

                       IF WS-ACCT-ID = LOW-VALUES
                          MOVE SPACE           TO ACCTIDO   OF COPAU0AO
                       ELSE
```
