```cobol
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

```
