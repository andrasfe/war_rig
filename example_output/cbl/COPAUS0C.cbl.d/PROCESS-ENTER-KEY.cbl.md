```cobol
                 PERFORM PROCESS-PAGE-FORWARD
              END-IF
           END-IF
           .


      *****************************************************************
       PROCESS-PF7-KEY.
      *****************************************************************

           IF CDEMO-CPVS-PAGE-NUM > 1
              COMPUTE CDEMO-CPVS-PAGE-NUM = CDEMO-CPVS-PAGE-NUM - 1

              MOVE CDEMO-CPVS-PAUKEY-PREV-PG(CDEMO-CPVS-PAGE-NUM)
                                           TO WS-AUTH-KEY-SAVE
              PERFORM GET-AUTH-SUMMARY

              SET SEND-ERASE-NO            TO TRUE

              SET NEXT-PAGE-YES            TO TRUE
              MOVE -1                      TO ACCTIDL OF COPAU0AI

              PERFORM INITIALIZE-AUTH-DATA

              PERFORM PROCESS-PAGE-FORWARD
           ELSE
              MOVE 'You are already at the top of the page...' TO
                               WS-MESSAGE
              SET SEND-ERASE-NO            TO TRUE
           END-IF
           .

      *****************************************************************
       PROCESS-PF8-KEY.
      *****************************************************************

           IF CDEMO-CPVS-PAUKEY-LAST = SPACES OR LOW-VALUES
               MOVE LOW-VALUES             TO WS-AUTH-KEY-SAVE
           ELSE
               MOVE CDEMO-CPVS-PAUKEY-LAST TO WS-AUTH-KEY-SAVE

               PERFORM GET-AUTH-SUMMARY
               PERFORM REPOSITION-AUTHORIZATIONS
           END-IF

           MOVE -1                         TO ACCTIDL OF COPAU0AI

           SET SEND-ERASE-NO               TO TRUE

           IF NEXT-PAGE-YES
               PERFORM INITIALIZE-AUTH-DATA

               PERFORM PROCESS-PAGE-FORWARD
           ELSE
               MOVE 'You are already at the bottom of the page...'
                                           TO WS-MESSAGE
           END-IF
           .

      *****************************************************************
       PROCESS-PAGE-FORWARD.
      *****************************************************************

           IF ERR-FLG-OFF

               MOVE 1             TO  WS-IDX

               MOVE LOW-VALUES    TO CDEMO-CPVS-PAUKEY-LAST

               PERFORM UNTIL WS-IDX > 5 OR AUTHS-EOF OR ERR-FLG-ON
                   IF EIBAID = DFHPF7 AND WS-IDX = 1
                      PERFORM REPOSITION-AUTHORIZATIONS
                   ELSE
                      PERFORM GET-AUTHORIZATIONS
                   END-IF
                   IF AUTHS-NOT-EOF AND ERR-FLG-OFF
                       PERFORM POPULATE-AUTH-LIST
                       COMPUTE WS-IDX = WS-IDX + 1

                       MOVE PA-AUTHORIZATION-KEY TO
                                             CDEMO-CPVS-PAUKEY-LAST
                       IF WS-IDX = 2
                          COMPUTE CDEMO-CPVS-PAGE-NUM =
                                  CDEMO-CPVS-PAGE-NUM + 1
                          MOVE PA-AUTHORIZATION-KEY TO
                          CDEMO-CPVS-PAUKEY-PREV-PG(CDEMO-CPVS-PAGE-NUM)
                       END-IF
                   END-IF
               END-PERFORM

               IF AUTHS-NOT-EOF AND ERR-FLG-OFF
                   PERFORM GET-AUTHORIZATIONS
                   IF AUTHS-NOT-EOF AND ERR-FLG-OFF
                       SET NEXT-PAGE-YES TO TRUE
                   ELSE
                       SET NEXT-PAGE-NO TO TRUE
                   END-IF
               END-IF

           END-IF.

```
