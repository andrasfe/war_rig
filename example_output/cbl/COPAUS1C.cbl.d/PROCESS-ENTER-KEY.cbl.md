```cobol
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

       PROCESS-PF8-KEY.

           MOVE CDEMO-ACCT-ID            TO WS-ACCT-ID
           MOVE CDEMO-CPVD-PAU-SELECTED  TO WS-AUTH-KEY

           PERFORM READ-AUTH-RECORD
           PERFORM READ-NEXT-AUTH-RECORD

           IF IMS-PSB-SCHD
              SET IMS-PSB-NOT-SCHD      TO TRUE
              PERFORM TAKE-SYNCPOINT
           END-IF

```
