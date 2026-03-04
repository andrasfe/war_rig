```cobol

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

              IF FOUND-PAUT-SMRY-SEG
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

```
