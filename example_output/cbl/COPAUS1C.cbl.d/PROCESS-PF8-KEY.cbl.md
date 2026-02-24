```cobol
       PROCESS-PF8-KEY.

           MOVE CDEMO-ACCT-ID            TO WS-ACCT-ID
           MOVE CDEMO-CPVD-PAU-SELECTED  TO WS-AUTH-KEY

           PERFORM READ-AUTH-RECORD
           PERFORM READ-NEXT-AUTH-RECORD

           IF IMS-PSB-SCHD
              SET IMS-PSB-NOT-SCHD      TO TRUE
              PERFORM TAKE-SYNCPOINT
           END-IF

           IF AUTHS-EOF
              SET SEND-ERASE-NO          TO TRUE
              MOVE 'Already at the last Authorization...'
                                         TO WS-MESSAGE
           ELSE
              MOVE PA-AUTHORIZATION-KEY  TO CDEMO-CPVD-PAU-SELECTED
              PERFORM POPULATE-AUTH-DETAILS
           END-IF
           .

```
