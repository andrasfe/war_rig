```cobol
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
           ELSE
              IF ACCTIDI OF COPAU0AI IS NOT NUMERIC
                MOVE LOW-VALUES               TO WS-ACCT-ID

                MOVE 'Y'                      TO WS-ERR-FLG
                MOVE
                'Acct Id must be Numeric ...' TO WS-MESSAGE

                MOVE -1                       TO ACCTIDL OF COPAU0AI

              ELSE
                MOVE ACCTIDI OF COPAU0AI      TO WS-ACCT-ID
                                                 CDEMO-ACCT-ID
                EVALUATE TRUE
                  WHEN SEL0001I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0001I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(1)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0002I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0002I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(2)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0003I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0003I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
```
