```cobol
       MAIN-PARA.
      *****************************************************************

           SET ERR-FLG-OFF TO TRUE
           SET AUTHS-NOT-EOF TO TRUE
           SET NEXT-PAGE-NO TO TRUE
           SET SEND-ERASE-YES TO TRUE

           MOVE SPACES TO WS-MESSAGE ERRMSGO OF COPAU0AO

           MOVE -1       TO ACCTIDL OF COPAU0AI

           IF EIBCALEN = 0
               INITIALIZE CARDDEMO-COMMAREA
               MOVE WS-PGM-AUTH-SMRY    TO CDEMO-TO-PROGRAM

               SET CDEMO-PGM-REENTER    TO TRUE
               MOVE LOW-VALUES          TO COPAU0AO
               MOVE -1                  TO ACCTIDL OF COPAU0AI

              PERFORM SEND-PAULST-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA

               IF NOT CDEMO-PGM-REENTER
                  SET CDEMO-PGM-REENTER     TO TRUE

                  MOVE LOW-VALUES           TO COPAU0AO

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
```
