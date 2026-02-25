```cobol
                           PERFORM SEND-AUTHVIEW-SCREEN
                       WHEN OTHER
                           PERFORM PROCESS-ENTER-KEY

                           MOVE CCDA-MSG-INVALID-KEY TO WS-MESSAGE
                           PERFORM SEND-AUTHVIEW-SCREEN
                   END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-CICS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC
           .

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

       MARK-AUTH-FRAUD.
           MOVE CDEMO-ACCT-ID            TO WS-ACCT-ID
           MOVE CDEMO-CPVD-PAU-SELECTED  TO WS-AUTH-KEY

           PERFORM READ-AUTH-RECORD

           IF PA-FRAUD-CONFIRMED
              SET PA-FRAUD-REMOVED          TO TRUE
              SET WS-REMOVE-FRAUD           TO TRUE
           ELSE
              SET PA-FRAUD-CONFIRMED        TO TRUE
              SET WS-REPORT-FRAUD           TO TRUE
           END-IF

           MOVE PENDING-AUTH-DETAILS        TO WS-FRAUD-AUTH-RECORD
           MOVE CDEMO-ACCT-ID               TO WS-FRD-ACCT-ID
           MOVE CDEMO-CUST-ID               TO WS-FRD-CUST-ID

           EXEC CICS LINK
                PROGRAM(WS-PGM-AUTH-FRAUD)
                COMMAREA(WS-FRAUD-DATA)
```
