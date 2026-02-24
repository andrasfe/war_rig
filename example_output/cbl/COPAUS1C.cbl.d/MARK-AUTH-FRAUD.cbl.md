```cobol
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
                NOHANDLE
           END-EXEC
           IF EIBRESP = DFHRESP(NORMAL)
              IF WS-FRD-UPDT-SUCCESS
                 PERFORM UPDATE-AUTH-DETAILS
              ELSE
                 MOVE WS-FRD-ACT-MSG     TO WS-MESSAGE
                 PERFORM ROLL-BACK
              END-IF
           ELSE
              PERFORM ROLL-BACK
           END-IF

           MOVE PA-AUTHORIZATION-KEY     TO CDEMO-CPVD-PAU-SELECTED
           PERFORM POPULATE-AUTH-DETAILS
           .

```
