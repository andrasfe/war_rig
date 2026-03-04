```cobol
                       END-IF

                       PERFORM SEND-PAULST-SCREEN
                     WHEN DFHPF3
                       MOVE WS-PGM-MENU        TO CDEMO-TO-PROGRAM
                       PERFORM RETURN-TO-PREV-SCREEN
                       PERFORM SEND-PAULST-SCREEN
                     WHEN DFHPF7
                       PERFORM PROCESS-PF7-KEY
                       PERFORM SEND-PAULST-SCREEN
                     WHEN DFHPF8
                       PERFORM PROCESS-PF8-KEY
                       PERFORM SEND-PAULST-SCREEN
                     WHEN OTHER
                       MOVE 'Y'              TO WS-ERR-FLG
                       MOVE -1               TO ACCTIDL OF COPAU0AI
                       MOVE CCDA-MSG-INVALID-KEY  TO WS-MESSAGE
                       PERFORM SEND-PAULST-SCREEN
                  END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-CICS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC.


      *****************************************************************
       PROCESS-ENTER-KEY.
      *****************************************************************

           IF ACCTIDI OF COPAU0AI = SPACES OR LOW-VALUES
              MOVE LOW-VALUES                 TO WS-ACCT-ID

              MOVE 'Y'                        TO WS-ERR-FLG
              MOVE
              'Please enter Acct Id...'       TO WS-MESSAGE

              MOVE -1                         TO ACCTIDL OF COPAU0AI
```
