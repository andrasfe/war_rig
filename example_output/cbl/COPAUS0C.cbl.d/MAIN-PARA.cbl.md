```cobol
                     WHEN DFHENTER
                       PERFORM PROCESS-ENTER-KEY

                       IF WS-ACCT-ID = LOW-VALUES
                          MOVE SPACE           TO ACCTIDO   OF COPAU0AO
                       ELSE
                          MOVE WS-ACCT-ID      TO ACCTIDO   OF COPAU0AO
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
                   MOVE CDEMO-CPVS-AUTH-KEYS(3)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0004I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0004I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(4)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0005I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0005I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(5)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN OTHER
                   MOVE SPACES   TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE SPACES   TO CDEMO-CPVS-PAU-SELECTED
                END-EVALUATE
                IF (CDEMO-CPVS-PAU-SEL-FLG NOT = SPACES AND LOW-VALUES)
                   AND
                   (CDEMO-CPVS-PAU-SELECTED NOT = SPACES AND LOW-VALUES)
                   EVALUATE CDEMO-CPVS-PAU-SEL-FLG
                     WHEN 'S'
                     WHEN 's'
                        MOVE WS-PGM-AUTH-DTL  TO CDEMO-TO-PROGRAM
                        MOVE WS-CICS-TRANID   TO CDEMO-FROM-TRANID
                        MOVE WS-PGM-AUTH-SMRY TO CDEMO-FROM-PROGRAM
                        MOVE 0                TO CDEMO-PGM-CONTEXT
                        SET CDEMO-PGM-ENTER   TO TRUE

                        EXEC CICS
                            XCTL PROGRAM(CDEMO-TO-PROGRAM)
                            COMMAREA(CARDDEMO-COMMAREA)
                        END-EXEC
                     WHEN OTHER
                       MOVE
                       'Invalid selection. Valid value is S'
                                              TO WS-MESSAGE
                       MOVE -1                TO ACCTIDL OF COPAU0AI
                   END-EVALUATE
                END-IF

              END-IF
           END-IF

           PERFORM GATHER-DETAILS
           .


      *****************************************************************
       GATHER-DETAILS.
      *****************************************************************

           MOVE -1       TO ACCTIDL OF COPAU0AI

           MOVE 0        TO CDEMO-CPVS-PAGE-NUM

           IF WS-ACCT-ID NOT = LOW-VALUES
              PERFORM GATHER-ACCOUNT-DETAILS

              PERFORM INITIALIZE-AUTH-DATA
```
