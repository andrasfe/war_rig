```cobol
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
```
