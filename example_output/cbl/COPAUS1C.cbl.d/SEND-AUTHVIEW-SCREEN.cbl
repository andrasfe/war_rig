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
